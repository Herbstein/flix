/*
 * Note that there is a specific bytecode instruction (SIPUSH) for pushing shorts (that aren't handled by BIPUSH).
 */

@test
def testInt16_01(): Int16 = -5320i16

@test
def testInt16_02(): Int16 = 4568i16

@test
def testInt16_03(): Int16 = -129i16

@test
def testInt16_04(): Int16 = 128i16

@test
def testInt16_05(): Int16 = 32767i16

@test
def testInt16_06(): Int16 = -32768i16

@test
def testInt16_07(): Int16 = 1_0i16

@test
def testInt16_08(): Int16 = 1_000i16

@test
def testInt16_09(): Int16 = 32_000i16

@test
def testInt16_10(): Int16 = 0b0101010101010101i16

@test
def testInt16_11(): Int16 = 0b01010101_01010101i16

@test
def testInt16_12(): Int16 = 0b0i16

@test
def testInt16_13(): Int16 = 0b1i16

@test
def testInt16_14(): Int16 = 0x1ABCi16

@test
def testInt16_15(): Int16 = 0x1A_BCi16

@test
def testInt16_16(): Int16 = 0x0i16

@test
def testInt16_17(): Int16 = 0x1i16

@test
def testInt16_18(): Int16 = 0x1abci16

@test
def testInt16_19(): Int16 = 0x1a_bci16
