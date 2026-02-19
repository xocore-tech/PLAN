const long page_size = 4096;
const long page_size_words = page_size / 8;

#define MAGIC_NUMBER 0x31764e414c50 // "PLANv1" in ASCII

#pragma pack(push, 1)       // Save alignment and set to 1 byte alignment
struct superblock {
    uint64_t magic;         // Magic number to identify our file type
    uint8_t sequence;       // Sequence number, wraps around 8-bit
    uint32_t val_checksum;  // Expected checksum for val.
    Val val;                // Tagged pointer into persistence heap.
    size_t write_offset;    // Current write position within the data area
    uint32_t checksum;      // Checksum of the superblock (excluding this field)
};
#pragma pack(pop)           // Restore previous alignment

struct superblock active_sb = {0};
off_t active_offset = 0;    // Binary location of current superblock
off_t inactive_offset = 0;  // Binary location of next superblock
uint32_t calculate_superblock_checksum(struct superblock *sb) {
  return calculate_crc32c((const char*)sb,
                          sizeof(struct superblock) - sizeof(uint32_t));
}
void init_persistence(int fd) {
  persistence_fd = fd;

  struct stat st;
  if (syscall_fstat(persistence_fd, &st) < 0) {
    syscall_close(persistence_fd);
    die("Failed to get file size");
  }
  filesize = st.st_size;

  off_t binary_end = elf_binary_end();
  off_t sb1_offset = round_up_align(binary_end, page_size);
  off_t sb2_offset = sb1_offset + page_size;

  // Data starts at the third page
  size_t data_start = sb1_offset + 2 * page_size;
  headersize = data_start;

  // Mount the data into memory. We do this before processing the superblocks
  // so we can do a top level item validation to make sure it was written
  // correctly.
  //
  // TODO: Lay out memory maps so they use all address space, collaborating
  // with the buddy allocator.
  const u64 rw = 3; // PROT_READ | PROT_WRITE
  const u64 map_flags = MAP_SHARED | MAP_FIXED;
  const u64 base_size = 1ULL << 39;
  persistence_start = syscall_mmap((u64*)base_addr,
                                   base_size, rw,
                                   map_flags, persistence_fd,
                                   data_start);

  // Read and check the validity of the two superblocks.
  struct superblock sb1, sb2;
  bool have_sb1 = false, have_sb2 = false;
  if (filesize >= sb1_offset + sizeof(struct superblock)) {
    have_sb1 = read_superblock(persistence_fd, sb1_offset, &sb1);
  }
  if (filesize >= sb2_offset + sizeof(struct superblock)) {
    have_sb2 = read_superblock(persistence_fd, sb2_offset, &sb2);
  }

  find_active_superblock(sb1, have_sb1, sb1_offset,
                         sb2, have_sb2, sb2_offset);

  persistence_cur = persistence_start + active_sb.write_offset;
  persistence_end = persistence_start + (base_size / 8);
}
bool read_superblock(int fd, off_t offset, struct superblock *sb) {
  ssize_t bytes_read = syscall_pread64(
      fd, (char*)sb, sizeof(struct superblock), offset);

  if (bytes_read != sizeof(struct superblock)) {
    if (bytes_read > 0 || bytes_read == -22 /* EINVAL */) {
      // This might be a new file or truncated file
      return false;
    }
    die("Failed to read superblock");
  }

  if (sb->magic != MAGIC_NUMBER) {
    printf("Invalid superblock (wrong magic number) at offset %ld\n",
           (long)offset);
    return false;  // Block is not valid
  }

  uint32_t expected_checksum = sb->checksum;
  uint32_t actual_checksum = calculate_superblock_checksum(sb);
  if (expected_checksum != actual_checksum) {
    printf("Superblock corruption detected (checksum mismatch) at offset %ld\n",
           (long)offset);
    return false;  // Block is not valid
  }

  expected_checksum = sb->val_checksum;
  actual_checksum = crc32_checksum_for(sb->val);
  if (expected_checksum != actual_checksum) {
    printf("Superblock corruption detected (content mismatch) at offset %ld\n",
           (long)offset);
    return false;  // Block is not valid
  }

  return true;
}
void write_superblock(int fd, off_t offset, struct superblock *sb) {
    sb->checksum = calculate_superblock_checksum(sb);

    ssize_t bytes_written = syscall_pwrite64(
        fd, (char*)sb, sizeof(struct superblock), offset);
    if (bytes_written != sizeof(struct superblock)) {
      die("Failed to write superblock");
    }

    if (syscall_fsync(fd) == -1) {
      die("Failed to fsync superblock");
    }

    // Swap offsets.
    off_t tmp = active_offset;
    active_offset = inactive_offset;
    inactive_offset = tmp;
}
void find_active_superblock(struct superblock sb1,
                            bool have_sb1,
                            off_t sb1_offset,
                            struct superblock sb2,
                            bool have_sb2,
                            off_t sb2_offset) {
  if (have_sb1 && have_sb2) {
    // Handle wraparound by checking if the difference (considering
    // unsigned overflow) is less than 128
    if ((uint8_t)(sb1.sequence - sb2.sequence) < 128) {
      active_sb = sb1;
      active_offset = sb1_offset;
      inactive_offset = sb2_offset;
    } else {
      active_sb = sb2;
      active_offset = sb2_offset;
      inactive_offset = sb1_offset;
    }
  } else if (have_sb1) {
    active_sb = sb1;
    active_offset = sb1_offset;
    inactive_offset = sb2_offset;
  } else if (have_sb2) {
    active_sb = sb2;
    active_offset = sb2_offset;
    inactive_offset = sb1_offset;
  } else {
    // Write the initial superblock
    active_sb.magic = MAGIC_NUMBER;
    active_sb.sequence = 1;
    active_sb.val_checksum = natural_crc32(0);
    active_sb.val = 0;
    active_sb.write_offset = 0;
    write_superblock(persistence_fd, sb1_offset, &active_sb);

    active_offset = sb1_offset;
    inactive_offset = sb2_offset;
  }
}
u64* allocate_persistence_space_for(size_t sz) {
  size_t required_size = headersize
                       + ((persistence_cur - persistence_start) * 8)
                       + (sz * 8);

  if (filesize < required_size) {
    size_t new_size = round_up_align(required_size, page_size);
    if (syscall_ftruncate(persistence_fd, new_size) == -1) {
      die("Failed to extend file");
    }

    filesize = new_size;
  }

  // Our current pointer is always page aligned.
  u64* now = persistence_cur;
  persistence_cur += round_up_align(sz, page_size_words);
  return now;
}

#define MS_SYNC 4

void msync_region(u64* begin, u64 word_size) {
  // Round word_size turned into bytes to the nearest page.
  u64 rounded_size = round_up_align(word_size * 8, page_size);

  // Sync the region
  if (syscall_msync(begin, rounded_size, MS_SYNC) == -1) {
    die("Failed to msync data");
  }
}
u64 natural_crc32(u64 nat) {
  __int64_t crc = 0xFFFFFFFF;
  crc = _mm_crc32_u64(crc, nat);
  crc ^= 0xFFFFFFFF;
  return crc;
}
u64 crc32_checksum_for(Val item) {
  if ((item >> 63) == 0) {
    return natural_crc32(item);
  } else {
    u64* ptr = PTR(item);
    u64 bitsz = get_bitsz(item);
    u64 wordsz = (bitsz + 63) / 64;

    return calculate_crc32c((char*)(ptr - 1), (wordsz + 1) * 8);
  }
}
void validate_item(Val item, u64 expected) {
  __int64_t crc = crc32_checksum_for(item);

  if ((item >> 63) == 1) {
    u64* ptr = PTR(item);
    if (ptr[-2] != crc) {
      printf("Item pointer CRC32 differs for %lx: ptr[-2]=%lx, actual=%lx\n",
             item, ptr[-2], crc);
      die("scrub failed");
    }
  }

  if (expected != crc) {
    printf("CRC32 differs for %lx: expected=%lx, actual=%lx\n",
           item, expected, crc);
    die("scrub failed");
  }
}
void scrub(Val item, u64 expected) {
  bump_alloc_t alloc;
  bump_alloc_init(&alloc, sizeof(rb_node));

  rb_tree tree;
  rb_init(&tree);

  scrub_item(item, expected, &tree, &alloc);

  bump_alloc_free(&alloc);
}
void scrub_item(Val item, u64 expected, rb_tree* tree, bump_alloc_t* a) {
  if ((item >> 63) == 0) {
    // Check a direct reference.
    u64 crc = natural_crc32(item);
    if (crc != expected) {
      printf("Header hash differs from expected for %lx\n", item);
      die("scrub failed");
    }

    return;
  }

  if (mark_item_in_tree(item, tree, a)) {
    return;
  }

  u64* ptr = PTR(item);
  if (ptr[-2] != expected) {
    printf("Header hash differs from expected for %lx\n", item);
    die("scrub failed");
  }

  validate_item(item, expected);

  u64 pin_count = ptr[2];
  if (ismegapin(item)) {
    u64 megapin_count = ptr[6 + 2 * pin_count];

    u32* pin_crcs = (u32*)(ptr + 6 + 2 * pin_count + 1 + 2 * megapin_count);

    // nonrecursively scrub the pins
    for (size_t i = 0; i < pin_count; ++i) {
      if (mark_item_in_tree(ptr[6 + i], tree, a))
        continue;

      validate_item(ptr[6 + i], pin_crcs[i]);
    }

    // recursively scrub the megapins.
    for (size_t i = 0; i < megapin_count; ++i) {
      scrub_item(ptr[6 + 2 * pin_count + 1 + i],
                 pin_crcs[pin_count + i],
                 tree, a);
    }
  } else {
    u32* pin_crcs = (u32*)(ptr + 6 + pin_count);
    for (size_t i = 0; i < pin_count; ++i) {
      scrub_item(ptr[6 + i], pin_crcs[i], tree, a);
    }
  }
}
