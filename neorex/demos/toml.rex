; This is a TOML document

title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = '1979-05-27T07:32:00-08:00

[database]
enabled = true
ports = [ 8000, 8001, 8002 ]
data = [ ["delta", "phi"], [3.14] ]
temp_targets = { cpu = 79.5, case = 72.0 }

[servers]

[servers.alpha]
ip = "10.0.0.1"
role = "frontend"

[servers.beta]
ip = "10.0.0.2"
role = "backend"

[multiline]

str1 = "I'm a string."
str2 = "You can ""quote"" me."
str3 = "Name\tJos\u00E9\nLoc\tSF."

str1 =
"""
Roses are red
Violets are blue
"""

str2 = """\
       The quick brown \
       fix jumps over \
       the lazy dog. \
       """

[literal.strings]
path = lit"C:\Users\nodejs\templates"
path2 = lit"\\User\admin$\system32"
quoted = lit"Tom ""Dubs"" Preston-Werner"
regex = lit"<\i\c*\s*>"

re = '''I [dw]on't need \d{2} apples'''

lines = '''
        The first newline is
        trimmed in raw strings.
        All other whitespace
        is preserved
        '''

; integers
int1 = +99
int2 = 42
int3 = 0
int4 = -17

; hexadecimal with prefix `0x`
hex1 = 0xDEADBEEF
hex2 = 0xdeadbeef
hex3 = 0xdead_beef

; octal with prefix `0o`
oct1 = 0o01234567
oct2 = 0o755

; binary with prefix `0b`
bin1 = 0b11010110

; fractional
float1 = +1.0
float2 = 3.1415
float3 = -0.01

; exponent
float4 = 5e+22
float5 = 1e06
float6 = -2E-2

; both
float7 = 6.626e-34

; separators
float8 = 224_617.445_991_228

\ infinity
infinite1 = inf  \ positive infinity
infinite2 = +inf \ positive infinity
infinite3 = -inf \ negative infinity

; not a number
not1 = nan
not2 = +nan
not3 = -nan

; no hyphens in unquoted keys.

'a-map-cannot:

  ? {be: expressed}
  : with a JSON pointer
