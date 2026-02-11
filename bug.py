import gdb
import io

u64_type = gdb.lookup_type("unsigned long long")

u64_ptr = u64_type.pointer()

buf    = io.StringIO()
r12    = 0
pdepth = 0

RED = "\033[31m"
GREEN = "\033[32m"
RESET = "\033[0m"

def write_bad(tag, off):
	print("badref(tag={:d}, off={:d})".format(tag, off))
	return

def write_pin(ptr, gcTy, bitSz, wordSz, cMeta):
	buf.write("<")
	if cMeta != 0:
		buf.write("(invalid tag)")
	if gcTy != 1:
		buf.write("(mismatched gc tag)")
	if bitSz != 384:
		buf.write("(bad size)")
	write_plan(ptr[0])
	buf.write(">")
	return

def write_nat(ptr, gcTy, bitSz, wordSz, cMeta):
	buf.write(f"{RED}nat{RESET}(...)")
	# buf.write(str((ptr, gcTy, bitSz, wordSz, cMeta)))
	# buf.write(")")
	return

def write_law(ptr, gcTy, bitSz, wordSz, cMeta):
	buf.write("{")
	buf.write("...")
	buf.write("}")
	return

def write_clz(ptr, gcTy, bitSz, wordSz, cMeta):
	write_plan(ptr[0])
	buf.write("[")
	args = wordSz - 1
	for i in range(1, args):
		if i > 1: buf.write(" ")
		write_plan(ptr[i])
	buf.write("]")
	return

def write_tnk(ptr, gcTy, bitSz, wordSz, cMeta):
	buf.write("(")
	for i in range(0, wordSz):
		if i > 0: buf.write(" ")
		write_plan(ptr[i])
	buf.write(")")
	return

def write_indirect(tag, off, head, ptr):
	cMeta  = tag & 0xff
	valTy  = tag >> 8
	gcTy   = head & 0xff
	bitSz  = head >> 8
	wordSz = (bitSz+63) // 64

	match valTy:
		case 0xc4: write_pin(ptr, gcTy, bitSz, wordSz, cMeta)
		case 0x82: write_nat(ptr, gcTy, bitSz, wordSz, cMeta)
		case 0xC8: write_law(ptr, gcTy, bitSz, wordSz, cMeta)
		case 0xD0: write_clz(ptr, gcTy, bitSz, wordSz, cMeta)
		case 0xE0: write_tnk(ptr, gcTy, bitSz, wordSz, cMeta)
		case _:    write_bad(tag, off)

def write_plan(val):
	if not(val >> 63):
		buf.write(str(val))
		return

	tag = val >> 48
	off = val & 0xffffffffffff # lower 48 bits

	hed = gdb.Value(r12 + ((off-1)*8)).cast(u64_ptr)
	ptr = gdb.Value(r12 + off*8).cast(u64_ptr)

	try:
		head = int(ptr[-1])
		write_indirect(tag, off, head, ptr)
	except gdb.error as e:
		return write_bad(tag, off)

class PLANPrint(gdb.Command):
	"""Pretty-print a PLAN value"""

	def __init__(self):
		super(PLANPrint, self).__init__("pt", gdb.COMMAND_USER)

	def invoke(self, arg, from_tty):
		global r12
		global buf

		args = arg.split()

		if not arg.strip() or len(args) == 0:
			print("Usage: pt expr..")
			return


		r12 = gdb.selected_frame().read_register("r12")
		for arg in args:
			buf = io.StringIO()
			write_plan(int(gdb.parse_and_eval(arg).cast(u64_type)))
			print(buf.getvalue())

cmd = PLANPrint()

print("[bug.py] Loaded custom GDB commands.");
