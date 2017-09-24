
from function_defs import deci, hexi, type_of, ints_of_4_byte_aligned_region, \
    string_to_dict, extend_zeroes, sign_16_bit_value, unsign_16_bit_value, \
    get_8_bit_ints_from_32_bit_int
from os.path import exists


OPCODE = 'OPCODE'  # For identifying instruction
EX_OPCODE = 'EX_OPCODE'  # For identifying instruction
OFFSET = 'OFFSET'  # Static values in which are represented as numbers  -- Numerical Parameter
ADDRESS = 'ADDRESS'  # Static values in which are represented as numbers  -- Numerical Parameter
IMMEDIATE = 'IMMEDIATE'  # Static values in which are represented as numbers  -- Numerical Parameter
BASE = 'BASE'  # Use as base before offset applies to target address  -- Register Parameter
RT = 'RT'  # General Purpose Register Target  -- Register Parameter
RD = 'RD'  # General Purpose Register Destination  -- Register Parameter
RS = 'RS'  # General Purpose Register Source  -- Register Parameter
FT = 'FT'  # Floating-Point General Purpose Register Target  -- Register Parameter
FD = 'FD'  # Floating-Point General Purpose Register Destination  -- Register Parameter
FS = 'FS'  # Floating-Point General Purpose Register Source  -- Register Parameter
CS = 'CS'  # Coprocessor-0 Register Source  -- Register Parameter
SA = 'SA'  # Shift Amount  -- Numerical Parameter
STYPE = 'STYPE'  # For identifying instruction
FMT = 'FMT'  # For identifying instruction
COND = 'COND'  # For identifying instruction
OP = 'OP'  # For identifying instruction
CC = 'CC'  # For identifying instruction
CO = 'CO'  # For identifying instruction
ES = 'ES'  # For identifying instruction
CODE_10 = 'CODE_10'  # Code (10bit)  -- Numerical Parameter
CODE_20 = 'CODE_20'  # Code (20bit)  -- Numerical Parameter

ADDRESS_ALIGNMENT = deci('04000000')  # In word-wise form. Byte-wise is 0x10000000

MAXIMUM_VALUES = {  # For Disassembler.encode(): How high the limit will be for immediate types of parameters
    'ADDRESS': deci('3FFFFFF'),
    'OFFSET': deci('FFFF'),
    'IMMEDIATE': deci('FFFF'),
    'CODE_20': deci('FFFFF'),
    'CODE_10': deci('3FF'),
    'OP': deci('1F'),
    'SA': deci('1F')
}

HEX_EXTEND = {  # For Disassembler.decode(): How many digits are required to represent each numerical value
    'ADDRESS': 8,
    'OFFSET': 8,
    'IMMEDIATE': 4,
    'CODE_20': 5,
    'CODE_10': 3,
    'OP': 2,
    'SA': 1
}

CODE_TYPES = {  # For Disassembler.decode(): The instances in which the parameters will translate to
    'ADDRESS': 'HEX',
    'OFFSET': 'HEX',
    'IMMEDIATE': 'HEX',
    'CODE_20': 'HEX',
    'CODE_10': 'HEX',
    'OP': 'HEX',
    'SA': 'HEX',
    'BASE': 'GENERAL_PURPOSE_REGISTER',
    'RT': 'GENERAL_PURPOSE_REGISTER',
    'RD': 'GENERAL_PURPOSE_REGISTER',
    'RS': 'GENERAL_PURPOSE_REGISTER',
    'FT': 'FLOATING_POINT_GENERAL_PURPOSE_REGISTER',
    'FD': 'FLOATING_POINT_GENERAL_PURPOSE_REGISTER',
    'FS': 'FLOATING_POINT_GENERAL_PURPOSE_REGISTER',
    'CS': 'CP0_REGISTER'
}

LENGTHS = {  # For Disassembler.fit(), how long each segment of data will be, in bits
    'ADDRESS': 26,
    'CODE_20': 20,
    'OFFSET': 16,
    'IMMEDIATE': 16,
    'CODE_10': 10,
    'OPCODE': 6,
    'EX_OPCODE': 5,
    'BASE': 5,
    'RT': 5,
    'RD': 5,
    'RS': 5,
    'FT': 5,
    'FD': 5,
    'FS': 5,
    'CS': 5,
    'SA': 5,
    'STYPE': 5,
    'FMT': 5,
    'OP': 5,
    'COND': 4,
    'CC': 3,
    'ES': 2,
    'CO': 1
}

REGISTERS = {  # For Disassembler.decode(): To obtain the names of decoded registers (register number == place in list indices)
    'GENERAL_PURPOSE_REGISTER': [
        'R0',
        'AT',
        'V0',
        'V1',
        'A0',
        'A1',
        'A2',
        'A3',
        'T0',
        'T1',
        'T2',
        'T3',
        'T4',
        'T5',
        'T6',
        'T7',
        'S0',
        'S1',
        'S2',
        'S3',
        'S4',
        'S5',
        'S6',
        'S7',
        'T8',
        'T9',
        'K0',
        'K1',
        'GP',
        'SP',
        'S8',
        'RA'
    ],
    'FLOATING_POINT_GENERAL_PURPOSE_REGISTER': [
        'F0',
        'F1',
        'F2',
        'F3',
        'F4',
        'F5',
        'F6',
        'F7',
        'F8',
        'F9',
        'F10',
        'F11',
        'F12',
        'F13',
        'F14',
        'F15',
        'F16',
        'F17',
        'F18',
        'F19',
        'F20',
        'F21',
        'F22',
        'F23',
        'F24',
        'F25',
        'F26',
        'F27',
        'F28',
        'F29',
        'F30',
        'F31'
    ],
    'CP0_REGISTER': [
        'INDEX',
        'RANDOM',
        'ENTRYLO0',
        'ENTRYLO1',
        'CONTEXT',
        'PAGEMASK',
        'WIRED',
        '*RESERVED0*',
        'BADVADDR',
        'COUNT',
        'ENTRYHI',
        'COMPARE',
        'STATUS',
        'CAUSE',
        'EXCEPTPC',
        'PREVID',
        'CONFIG',
        'LLADDR',
        'WATCHLO',
        'WATCHHI',
        'XCONTEXT',
        '*RESERVED1*',
        '*RESERVED2*',
        '*RESERVED3*',
        '*RESERVED4*',
        '*RESERVED5*',
        'PERR',
        'CACHEERR',
        'TAGLO',
        'TAGHI',
        'ERROREPC',
        '*RESERVED6*'
    ]
}

REGISTERS_ENCODE = {  # For Disassembler.encode(): To pull the values of register names in order to encode
    'R0': 0,
    'AT': 1,
    'V0': 2,
    'V1': 3,
    'A0': 4,
    'A1': 5,
    'A2': 6,
    'A3': 7,
    'T0': 8,
    'T1': 9,
    'T2': 10,
    'T3': 11,
    'T4': 12,
    'T5': 13,
    'T6': 14,
    'T7': 15,
    'S0': 16,
    'S1': 17,
    'S2': 18,
    'S3': 19,
    'S4': 20,
    'S5': 21,
    'S6': 22,
    'S7': 23,
    'T8': 24,
    'T9': 25,
    'K0': 26,
    'K1': 27,
    'GP': 28,
    'SP': 29,
    'S8': 30,
    'RA': 31,
    'F0': 0,
    'F1': 1,
    'F2': 2,
    'F3': 3,
    'F4': 4,
    'F5': 5,
    'F6': 6,
    'F7': 7,
    'F8': 8,
    'F9': 9,
    'F10': 10,
    'F11': 11,
    'F12': 12,
    'F13': 13,
    'F14': 14,
    'F15': 15,
    'F16': 16,
    'F17': 17,
    'F18': 18,
    'F19': 19,
    'F20': 20,
    'F21': 21,
    'F22': 22,
    'F23': 23,
    'F24': 24,
    'F25': 25,
    'F26': 26,
    'F27': 27,
    'F28': 28,
    'F29': 29,
    'F30': 30,
    'F31': 31,
    'INDEX': 0,
    'RANDOM': 1,
    'ENTRYLO0': 2,
    'ENTRYLO1': 3,
    'CONTEXT': 4,
    'PAGEMASK': 5,
    'WIRED': 6,
    '*RESERVED0*': 7,
    'BADVADDR': 8,
    'COUNT': 9,
    'ENTRYHI': 10,
    'COMPARE': 11,
    'STATUS': 12,
    'CAUSE': 13,
    'EXCEPTPC': 14,
    'PREVID': 15,
    'CONFIG': 16,
    'LLADDR': 17,
    'WATCHLO': 18,
    'WATCHHI': 19,
    'XCONTEXT': 20,
    '*RESERVED1*': 21,
    '*RESERVED2*': 22,
    '*RESERVED3*': 23,
    '*RESERVED4*': 24,
    '*RESERVED5*': 25,
    'PERR': 26,
    'CACHEERR': 27,
    'TAGLO': 28,
    'TAGHI': 29,
    'ERROREPC': 30,
    '*RESERVED6*': 31
    # Not sure if RESERVED are actually registers or not.
}


class Disassembler:
    def __init__(self, base_file_name='', hacked_file_name=''):

        base_part = base_file_name.rfind('\\') + 1
        self.base_folder = base_file_name[:base_part]
        self.base_file_name = base_file_name[base_part:]
        with open(base_file_name, 'rb') as file:
            self.base_file = bytearray(file.read())
        if self.base_file[2:4].hex() == '3780':
            self.base_file = bytearray(ints_of_4_byte_aligned_region(self.base_file, byteorder='little'))
        elif self.base_file[0:2].hex() != '8037':
            self.nothing = None
            # todo: "not a rom" error

        hack_part = hacked_file_name.rfind('\\') + 1
        self.hack_folder = hacked_file_name[:hack_part]
        self.hack_file_name = hacked_file_name[hack_part:]
        with open(hacked_file_name, 'rb') as file:
            self.hack_file = bytearray(file.read())
        if self.hack_file[2:4].hex() == '3780':
            self.hack_file = bytearray(ints_of_4_byte_aligned_region(self.hack_file, byteorder='little'))
        elif self.hack_file[0:2].hex() != '8037':
            self.nothing = None
            # todo: "not a rom" error

        self.header_items = {
            # Section labeled    data_start    data_end (not inclusive)
            'Rom Validate':     [deci('0000'), deci('0002')],
            'Rom Compressed':    deci('0002'),
            '400000000F':       [deci('0003'), deci('0008')],
            'Game Offset':      [deci('0008'), deci('000C')],
            '000014':           [deci('000C'), deci('000F')],
            'Game-Specific?':    deci('000F'),
            'CRC1':             [deci('0010'), deci('0014')],
            'CRC2':             [deci('0014'), deci('0018')],
            '0000000000000000': [deci('0018'), deci('0020')],
            'Rom Name':         [deci('0020'), deci('0034')],
            '00000000000000':   [deci('0034'), deci('003B')],
            'Manufacturer ID':   deci('003B'),
            'Cartridge ID':     [deci('003C'), deci('003E')],
            'Country Code':      deci('003E'),
            '00':                deci('003F'),
            'Boot Code':        [deci('0040'), deci('1000')],
            'Game Code':         deci('1000')
        }

        self.comments_file = '{}.comments'.format(self.hack_folder + self.hack_file_name)
        self.comments = {}
        if not exists(self.comments_file):
            # Start new comments off with some header labels
            for i in self.header_items.keys():
                if type_of(self.header_items[i]) == 'list':
                    comments_location = '{}'.format((self.header_items[i][0] >> 2))
                else:
                    comments_location = '{}'.format((self.header_items[i] >> 2))
                already_data = comments_location in self.comments.keys()
                if already_data:
                    self.comments[comments_location] = self.comments[comments_location] + ' | ' + i
                else:
                    self.comments[comments_location] = i
        else:
            # Load the existing comments
            with open(self.comments_file, 'r') as file:
                self.comments = string_to_dict(file.read())

        # Display the rom name in a user-readable format
        segment = self.hack_file[self.header_items['Rom Name'][0]: self.header_items['Rom Name'][1]]
        self.comments['9'] = segment.decode()

        # Save the game offset
        segment = self.hack_file[self.header_items['Game Offset'][0]: self.header_items['Game Offset'][1]]
        self.game_offset = int.from_bytes(segment, byteorder='big', signed=False) - deci('1000')

        self.game_address_mode = False
        self.immediate_identifier = '$'

        self.mnemonics = []
        self.encodes = []
        self.appearances = []
        # self.appearance_bit_correspondences = []
        self.comparable_bits = []
        self.identifying_bits = []
        self.amount = 0
        self.file_length = len(self.base_file)

        ''' Format: fit(mnemonic, encoding, appearance)
            mnemonic: String
                The shortened name of the instruction. 

            encoding: [segment1, segment2, ...] where segments are either: Int or String or [String, Int]
                An array containing segments which detail the resultant bit values for the decoder to bitwise & check against.
                Every segment will contain the segment's length in bits, and also whether or not the correlating segment 
                  within the values coming from the assembly should identify as the instruction.
                The bit-lengths for all instructions will total 32.

                String segments (all variables used here are predefined strings)
                  - Means this segment is irrelevant for identifying this instruction, and also means the appearance list
                      is allowed to contain this string, as it is a variable parameter for the instruction.
                  - The bit length for the segment is taken from the LENGTHS dictionary using the string as the key.

                Int segments
                  - Means this number is the length of the bit-segment, and that correlating bit segments
                      must equal all 0's in order to identify with this instruction.

                List [String, Int] segments
                  - Means that the current bit segment is Length long, and correlating bit segments must equal to Int
                      in order to identify with this instruction. Length is pulled from LENGTHS just like it is for
                      String segments.

            Appearance: [String1, String2, ...]
                Is used for encoding/decoding the parameters of instructions based on the order they appear in the actual syntax.
                The fitment will use each string to assort the bit-locations for each parameter within each 32-bit code 
                  segment found within the files we are disassembling.
                Appearance can be an empty list for codes that do not have parameters.


            For instructions that are miss-interpreted due to parameters spoofing the values of other opcodes,
              they are simply placed "before" the instruction in which it was miss-interpreted to in the 'self.fit()'
              section, so that the decoder checks the miss-interpreted instruction before checking the one that can
              cause the miss-interpretation.


            NOP cannot be fitted, as its value is 0. (this is the case for MIPS I, II and III, at least)
            The decoder will pick everything up as NOP because it will bitwise & every code with 0 to always equal 0


            The fitment parameters only "look" like the MIPS III documentation, but a few things have been changed to 
              better accommodate the encoding and decoding process.
              
            
            This is probably not the authentic way to disassemble (I have no idea - I am still learning), but it works.
        '''

        # Load and Store instructions
        self.fit('LB',    [[OPCODE, 32], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('LBU',   [[OPCODE, 36], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('LD',    [[OPCODE, 55], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('LDL',   [[OPCODE, 26], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('LDR',   [[OPCODE, 27], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('LH',    [[OPCODE, 33], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('LHU',   [[OPCODE, 37], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('LL',    [[OPCODE, 48], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('LLD',   [[OPCODE, 52], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('LW',    [[OPCODE, 35], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('LWL',   [[OPCODE, 34], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('LWR',   [[OPCODE, 38], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('LWU',   [[OPCODE, 39], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('SB',    [[OPCODE, 40], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('SC',    [[OPCODE, 56], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('SCD',   [[OPCODE, 60], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('SD',    [[OPCODE, 63], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('SDL',   [[OPCODE, 44], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('SDR',   [[OPCODE, 45], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('SH',    [[OPCODE, 41], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('SW',    [[OPCODE, 43], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('SWL',   [[OPCODE, 42], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('SWR',   [[OPCODE, 46], BASE, RT, IMMEDIATE],  [RT, IMMEDIATE, BASE])
        self.fit('SYNC',  [6, 15, STYPE, [OPCODE, 15]],         [])

        # Arithmetic Instructions
        self.fit('ADD',     [6, RS, RT, RD, 5, [OPCODE, 32]],   [RD, RS, RT])
        self.fit('ADDI',    [[OPCODE, 8], RS, RT, IMMEDIATE],   [RT, RS, IMMEDIATE])
        self.fit('ADDIU',   [[OPCODE, 9], RS, RT, IMMEDIATE],   [RT, RS, IMMEDIATE])
        self.fit('ADDU',    [6, RS, RT, RD, 5, [OPCODE, 33]],   [RD, RS, RT])
        self.fit('AND',     [6, RS, RT, RD, 5, [OPCODE, 36]],   [RD, RS, RT])
        self.fit('ANDI',    [[OPCODE, 12], RS, RT, IMMEDIATE],  [RT, RS, IMMEDIATE])
        self.fit('DADD',    [6, RS, RT, RD, 5, [OPCODE, 44]],   [RD, RS, RT])
        self.fit('DADDI',   [[OPCODE, 24], RS, RT, IMMEDIATE],  [RT, RS, IMMEDIATE])
        self.fit('DADDIU',  [[OPCODE, 25], RS, RT, IMMEDIATE],  [RT, RS, IMMEDIATE])
        self.fit('DADDU',   [6, RS, RT, RD, 5, [OPCODE, 45]],   [RD, RS, RT])
        self.fit('DDIV',    [6, RS, RT, 10, [OPCODE, 30]],      [RS, RT])
        self.fit('DDIVU',   [6, RS, RT, 10, [OPCODE, 31]],      [RS, RT])
        self.fit('DIV',     [6, RS, RT, 10, [OPCODE, 26]],      [RS, RT])
        self.fit('DIVU',    [6, RS, RT, 10, [OPCODE, 27]],      [RS, RT])
        self.fit('DMULT',   [6, RS, RT, 10, [OPCODE, 28]],      [RS, RT])
        self.fit('DMULTU',  [6, RS, RT, 10, [OPCODE, 29]],      [RS, RT])
        self.fit('DSLL',    [6, 5, RT, RD, SA, [OPCODE, 56]],   [RD, RT, SA])
        self.fit('DSLL32',  [6, 5, RT, RD, SA, [OPCODE, 60]],   [RD, RT, SA])
        self.fit('DSLLV',   [6, RS, RT, RD, 5, [OPCODE, 20]],   [RD, RT, RS])
        self.fit('DSRA',    [6, 5, RT, RD, SA, [OPCODE, 59]],   [RD, RT, SA])
        self.fit('DSRA32',  [6, 5, RT, RD, SA, [OPCODE, 63]],   [RD, RT, SA])
        self.fit('DSRAV',   [6, RS, RT, RD, 5, [OPCODE, 23]],   [RD, RT, RS])
        self.fit('DSRL',    [6, 5, RT, RD, SA, [OPCODE, 58]],   [RD, RT, SA])
        self.fit('DSRL32',  [6, 5, RT, RD, SA, [OPCODE, 62]],   [RD, RT, SA])
        self.fit('DSRLV',   [6, RS, RT, RD, 5, [OPCODE, 22]],   [RD, RT, RS])
        self.fit('DSUB',    [6, RS, RT, RD, 5, [OPCODE, 46]],   [RD, RS, RT])
        self.fit('DSUBU',   [6, RS, RT, RD, 5, [OPCODE, 47]],   [RD, RS, RT])
        self.fit('LUI',     [[OPCODE, 15], 5, RT, IMMEDIATE],   [RT, IMMEDIATE])
        self.fit('MFHI',    [6, 10, RD, 5, [OPCODE, 16]],       [RD])
        self.fit('MFLO',    [6, 10, RD, 5, [OPCODE, 18]],       [RD])
        self.fit('MTHI',    [6, RS, 15, [OPCODE, 17]],          [RS])
        self.fit('MTLO',    [6, RS, 15, [OPCODE, 19]],          [RS])
        self.fit('MULT',    [6, RS, RT, 10, [OPCODE, 24]],      [RS, RT])
        self.fit('MULTU',   [6, RS, RT, 10, [OPCODE, 25]],      [RS, RT])
        self.fit('NOR',     [6, RS, RT, RD, 5, [OPCODE, 39]],   [RD, RS, RT])
        self.fit('OR',      [6, RS, RT, RD, 5, [OPCODE, 37]],   [RD, RS, RT])
        self.fit('ORI',     [[OPCODE, 13], RS, RT, IMMEDIATE],  [RT, RS, IMMEDIATE])
        self.fit('SLL',     [6, 5, RT, RD, SA, 6],              [RD, RT, SA])
        self.fit('SLLV',    [6, RS, RT, RD, 5, [OPCODE, 4]],    [RD, RT, RS])
        self.fit('SLT',     [6, RS, RT, RD, 5, [OPCODE, 42]],   [RD, RS, RT])
        self.fit('SLTI',    [[OPCODE, 10], RS, RT, IMMEDIATE],  [RT, RS, IMMEDIATE])
        self.fit('SLTIU',   [[OPCODE, 11], RS, RT, IMMEDIATE],  [RT, RS, IMMEDIATE])
        self.fit('SLTU',    [6, RS, RT, RD, 5, [OPCODE, 43]],   [RD, RS, RT])
        self.fit('SRA',     [6, 5, RT, RD, SA, [OPCODE, 3]],    [RD, RT, SA])
        self.fit('SRAV',    [6, RS, RT, RD, 5, [OPCODE, 7]],    [RD, RT, RS])
        self.fit('SRL',     [6, 5, RT, RD, SA, [OPCODE, 2]],    [RD, RT, SA])
        self.fit('SRLV',    [6, RS, RT, RD, 5, [OPCODE, 6]],    [RD, RT, RS])
        self.fit('SUB',     [6, RS, RT, RD, 5, [OPCODE, 34]],   [RD, RS, RT])
        self.fit('SUBU',    [6, RS, RT, RD, 5, [OPCODE, 35]],   [RD, RS, RT])
        self.fit('XOR',     [6, RS, RT, RD, 5, [OPCODE, 38]],   [RD, RS, RT])
        self.fit('XORI',    [[OPCODE, 14], RS, RT, IMMEDIATE],  [RT, RS, IMMEDIATE])

        # Jump and Branch Instructions
        self.fit('BEQ',      [[OPCODE, 4], RS, RT, OFFSET],               [RS, RT, OFFSET])
        self.fit('BEQL',     [[OPCODE, 20], RS, RT, OFFSET],              [RS, RT, OFFSET])
        self.fit('BGEZ',     [[OPCODE, 1], RS, [EX_OPCODE, 1], OFFSET],   [RS, OFFSET])
        self.fit('BGEZAL',   [[OPCODE, 1], RS, [EX_OPCODE, 17], OFFSET],  [RS, OFFSET])
        self.fit('BGEZALL',  [[OPCODE, 1], RS, [EX_OPCODE, 19], OFFSET],  [RS, OFFSET])
        self.fit('BGEZL',    [[OPCODE, 1], RS, [EX_OPCODE, 3], OFFSET],   [RS, OFFSET])
        self.fit('BGTZ',     [[OPCODE, 7], RS, 5, OFFSET],                [RS, OFFSET])
        self.fit('BGTZL',    [[OPCODE, 23], RS, 5, OFFSET],               [RS, OFFSET])
        self.fit('BLEZ',     [[OPCODE, 6], RS, 5, OFFSET],                [RS, OFFSET])
        self.fit('BLEZL',    [[OPCODE, 22], RS, 5, OFFSET],               [RS, OFFSET])
        self.fit('BLTZ',     [[OPCODE, 1], RS, 5, OFFSET],                [RS, OFFSET])
        self.fit('BLTZAL',   [[OPCODE, 1], RS, [EX_OPCODE, 16], OFFSET],  [RS, OFFSET])
        self.fit('BLTZALL',  [[OPCODE, 1], RS, [EX_OPCODE, 18], OFFSET],  [RS, OFFSET])
        self.fit('BLTZL',    [[OPCODE, 1], RS, [EX_OPCODE, 2], OFFSET],   [RS, OFFSET])
        self.fit('BNEZ',     [[OPCODE, 5], RS, 5, OFFSET],                [RS, OFFSET])
        self.fit('BNEL',     [[OPCODE, 21], RS, RT, OFFSET],              [RS, RT, OFFSET])
        self.fit('BNE',      [[OPCODE, 5], RS, RT, OFFSET],               [RS, RT, OFFSET])
        self.fit('J',        [[OPCODE, 2], ADDRESS],                      [ADDRESS])
        self.fit('JAL',      [[OPCODE, 3], ADDRESS],                      [ADDRESS])
        self.fit('JALR',     [6, RS, 5, RD, 5, [OPCODE, 9]],              [RD, RS])
        self.fit('JR',       [6, RS, 15, [OPCODE, 8]],                    [RS])

        # Special Instructions
        self.fit('BREAK',    [6, CODE_20, [OPCODE, 13]],  [CODE_20])
        self.fit('SYSCALL',  [6, CODE_20, [OPCODE, 12]],  [CODE_20])

        # Exception Instructions
        self.fit('TEQ',    [6, RS, RT, CODE_10, [OPCODE, 52]],             [CODE_10, RS, RT])
        self.fit('TEQI',   [[OPCODE, 1], RS, [EX_OPCODE, 12], IMMEDIATE],  [RS, IMMEDIATE])
        self.fit('TGE',    [6, RS, RT, CODE_10, [OPCODE, 48]],             [CODE_10, RS, RT])
        self.fit('TGEI',   [[OPCODE, 1], RS, [EX_OPCODE, 8], IMMEDIATE],   [RS, IMMEDIATE])
        self.fit('TGEIU',  [[OPCODE, 1], RS, [EX_OPCODE, 9], IMMEDIATE],   [RS, IMMEDIATE])
        self.fit('TGEU',   [6, RS, RT, CODE_10, [OPCODE, 49]],             [CODE_10, RS, RT])
        self.fit('TLT',    [6, RS, RT, CODE_10, [OPCODE, 50]],             [CODE_10, RS, RT])
        self.fit('TLTI',   [[OPCODE, 1], RS, [EX_OPCODE, 10], IMMEDIATE],  [RS, IMMEDIATE])
        self.fit('TLTIU',  [[OPCODE, 1], RS, [EX_OPCODE, 11], IMMEDIATE],  [RS, IMMEDIATE])
        self.fit('TLTU',   [6, RS, RT, CODE_10, [OPCODE, 51]],             [CODE_10, RS, RT])
        self.fit('TNE',    [6, RS, RT, CODE_10, [OPCODE, 54]],             [CODE_10, RS, RT])
        self.fit('TNEI',   [[OPCODE, 1], RS, [EX_OPCODE, 14], IMMEDIATE],  [RS, IMMEDIATE])

        # System Control Processor (COP0) Instructions
        self.fit('CACHE',  [[OPCODE, 47], BASE, OP, IMMEDIATE],         [OP, IMMEDIATE, BASE])
        self.fit('DMFC0',  [[OPCODE, 16], [EX_OPCODE, 1], RT, CS, 11],  [RT, CS])
        self.fit('DMTC0',  [[OPCODE, 16], [EX_OPCODE, 5], RT, CS, 11],  [RT, CS])
        self.fit('ERET',   [[OPCODE, 16], CO, 19, [OPCODE, 24]],        [])
        self.fit('MFC0',   [[OPCODE, 16], 5, RT, CS, 11],               [RT, CS])
        self.fit('MTC0',   [[OPCODE, 16], [EX_OPCODE, 4], RT, CS, 11],  [RT, CS])
        self.fit('TLBP',   [[OPCODE, 16], CO, 19, [OPCODE, 8]],         [])
        self.fit('TLBR',   [[OPCODE, 16], CO, 19, [OPCODE, 1]],         [])
        self.fit('TLBWI',  [[OPCODE, 16], CO, 19, [OPCODE, 2]],         [])
        self.fit('TLBWR',  [[OPCODE, 16], CO, 19, [OPCODE, 6]],         [])

        # Floating-point Unit (COP1) Instructions
        self.fit('C.F.S',     [[OPCODE, 17], [FMT, 16], FT, FS, [CC, 0], 2, [ES, 3], [COND, 0]],   [FS, FT])
        self.fit('C.UN.S',    [[OPCODE, 17], [FMT, 16], FT, FS, [CC, 0], 2, [ES, 3], [COND, 1]],   [FS, FT])
        self.fit('C.EQ.S',    [[OPCODE, 17], [FMT, 16], FT, FS, [CC, 0], 2, [ES, 3], [COND, 2]],   [FS, FT])
        self.fit('C.UEQ.S',   [[OPCODE, 17], [FMT, 16], FT, FS, [CC, 0], 2, [ES, 3], [COND, 3]],   [FS, FT])
        self.fit('C.OLT.S',   [[OPCODE, 17], [FMT, 16], FT, FS, [CC, 0], 2, [ES, 3], [COND, 4]],   [FS, FT])
        self.fit('C.ULT.S',   [[OPCODE, 17], [FMT, 16], FT, FS, [CC, 0], 2, [ES, 3], [COND, 5]],   [FS, FT])
        self.fit('C.OLE.S',   [[OPCODE, 17], [FMT, 16], FT, FS, [CC, 0], 2, [ES, 3], [COND, 6]],   [FS, FT])
        self.fit('C.ULE.S',   [[OPCODE, 17], [FMT, 16], FT, FS, [CC, 0], 2, [ES, 3], [COND, 7]],   [FS, FT])
        self.fit('C.SF.S',    [[OPCODE, 17], [FMT, 16], FT, FS, [CC, 0], 2, [ES, 3], [COND, 8]],   [FS, FT])
        self.fit('C.NGLE.S',  [[OPCODE, 17], [FMT, 16], FT, FS, [CC, 0], 2, [ES, 3], [COND, 9]],   [FS, FT])
        self.fit('C.SEQ.S',   [[OPCODE, 17], [FMT, 16], FT, FS, [CC, 0], 2, [ES, 3], [COND, 10]],  [FS, FT])
        self.fit('C.NGL.S',   [[OPCODE, 17], [FMT, 16], FT, FS, [CC, 0], 2, [ES, 3], [COND, 11]],  [FS, FT])
        self.fit('C.LT.S',    [[OPCODE, 17], [FMT, 16], FT, FS, [CC, 0], 2, [ES, 3], [COND, 12]],  [FS, FT])
        self.fit('C.NGE.S',   [[OPCODE, 17], [FMT, 16], FT, FS, [CC, 0], 2, [ES, 3], [COND, 13]],  [FS, FT])
        self.fit('C.LE.S',    [[OPCODE, 17], [FMT, 16], FT, FS, [CC, 0], 2, [ES, 3], [COND, 14]],  [FS, FT])
        self.fit('C.NGT.S',   [[OPCODE, 17], [FMT, 16], FT, FS, [CC, 0], 2, [ES, 3], [COND, 15]],  [FS, FT])
        self.fit('C.F.D',     [[OPCODE, 17], [FMT, 17], FT, FS, [CC, 0], 2, [ES, 3], [COND, 0]],   [FS, FT])
        self.fit('C.UN.D',    [[OPCODE, 17], [FMT, 17], FT, FS, [CC, 0], 2, [ES, 3], [COND, 1]],   [FS, FT])
        self.fit('C.EQ.D',    [[OPCODE, 17], [FMT, 17], FT, FS, [CC, 0], 2, [ES, 3], [COND, 2]],   [FS, FT])
        self.fit('C.UEQ.D',   [[OPCODE, 17], [FMT, 17], FT, FS, [CC, 0], 2, [ES, 3], [COND, 3]],   [FS, FT])
        self.fit('C.OLT.D',   [[OPCODE, 17], [FMT, 17], FT, FS, [CC, 0], 2, [ES, 3], [COND, 4]],   [FS, FT])
        self.fit('C.ULT.D',   [[OPCODE, 17], [FMT, 17], FT, FS, [CC, 0], 2, [ES, 3], [COND, 5]],   [FS, FT])
        self.fit('C.OLE.D',   [[OPCODE, 17], [FMT, 17], FT, FS, [CC, 0], 2, [ES, 3], [COND, 6]],   [FS, FT])
        self.fit('C.ULE.D',   [[OPCODE, 17], [FMT, 17], FT, FS, [CC, 0], 2, [ES, 3], [COND, 7]],   [FS, FT])
        self.fit('C.SF.D',    [[OPCODE, 17], [FMT, 17], FT, FS, [CC, 0], 2, [ES, 3], [COND, 8]],   [FS, FT])
        self.fit('C.NGLE.D',  [[OPCODE, 17], [FMT, 17], FT, FS, [CC, 0], 2, [ES, 3], [COND, 9]],   [FS, FT])
        self.fit('C.SEQ.D',   [[OPCODE, 17], [FMT, 17], FT, FS, [CC, 0], 2, [ES, 3], [COND, 10]],  [FS, FT])
        self.fit('C.NGL.D',   [[OPCODE, 17], [FMT, 17], FT, FS, [CC, 0], 2, [ES, 3], [COND, 11]],  [FS, FT])
        self.fit('C.LT.D',    [[OPCODE, 17], [FMT, 17], FT, FS, [CC, 0], 2, [ES, 3], [COND, 12]],  [FS, FT])
        self.fit('C.NGE.D',   [[OPCODE, 17], [FMT, 17], FT, FS, [CC, 0], 2, [ES, 3], [COND, 13]],  [FS, FT])
        self.fit('C.LE.D',    [[OPCODE, 17], [FMT, 17], FT, FS, [CC, 0], 2, [ES, 3], [COND, 14]],  [FS, FT])
        self.fit('C.NGT.D',   [[OPCODE, 17], [FMT, 17], FT, FS, [CC, 0], 2, [ES, 3], [COND, 15]],  [FS, FT])

        # Floating-point Unit (COP1) Instructions continued...
        self.fit('ABS.S',      [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 5]],         [FD, FS])
        self.fit('ABS.D',      [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 5]],         [FD, FS])
        self.fit('ADD.S',      [[OPCODE, 17], [FMT, 16], FT, FS, FD, 6],                  [FD, FS, FT])
        self.fit('ADD.D',      [[OPCODE, 17], [FMT, 17], FT, FS, FD, 6],                  [FD, FS, FT])
        self.fit('BC1F',       [[OPCODE, 17], [EX_OPCODE, 8], [CC, 0], [ES, 0], OFFSET],  [OFFSET])
        self.fit('BC1FL',      [[OPCODE, 17], [EX_OPCODE, 8], [CC, 0], [ES, 2], OFFSET],  [OFFSET])
        self.fit('BC1T',       [[OPCODE, 17], [EX_OPCODE, 8], [CC, 0], [ES, 1], OFFSET],  [OFFSET])
        self.fit('BC1TL',      [[OPCODE, 17], [EX_OPCODE, 8], [CC, 0], [ES, 3], OFFSET],  [OFFSET])
        self.fit('CEIL.L.S',   [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 10]],        [FD, FS])
        self.fit('CEIL.L.D',   [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 10]],        [FD, FS])
        self.fit('CEIL.W.S',   [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 14]],        [FD, FS])
        self.fit('CEIL.W.D',   [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 14]],        [FD, FS])
        self.fit('CFC1',       [[OPCODE, 17], [EX_OPCODE, 2], RT, FS, 11],                [RT, FS])
        self.fit('CTC1',       [[OPCODE, 17], [EX_OPCODE, 6], RT, FS, 11],                [RT, FS])
        self.fit('CVT.D.S',    [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 33]],        [FD, FS])
        self.fit('CVT.D.W',    [[OPCODE, 17], [FMT, 20], 5, FS, FD, [OPCODE, 33]],        [FD, FS])
        self.fit('CVT.D.L',    [[OPCODE, 17], [FMT, 21], 5, FS, FD, [OPCODE, 33]],        [FD, FS])
        self.fit('CVT.L.S',    [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 37]],        [FD, FS])
        self.fit('CVT.L.D',    [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 37]],        [FD, FS])
        self.fit('CVT.S.D',    [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 32]],        [FD, FS])
        self.fit('CVT.S.W',    [[OPCODE, 17], [FMT, 20], 5, FS, FD, [OPCODE, 32]],        [FD, FS])
        self.fit('CVT.S.L',    [[OPCODE, 17], [FMT, 21], 5, FS, FD, [OPCODE, 32]],        [FD, FS])
        self.fit('CVT.W.S',    [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 36]],        [FD, FS])
        self.fit('CVT.W.D',    [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 36]],        [FD, FS])
        self.fit('DIV.S',      [[OPCODE, 17], [FMT, 16], FT, FS, FD, [OPCODE, 3]],        [FD, FS, FT])
        self.fit('DIV.D',      [[OPCODE, 17], [FMT, 17], FT, FS, FD, [OPCODE, 3]],        [FD, FS, FT])
        self.fit('DMFC1',      [[OPCODE, 17], [EX_OPCODE, 1], RT, FS, 11],                [RT, FS])
        self.fit('DMTC1',      [[OPCODE, 17], [EX_OPCODE, 5], RT, FS, 11],                [RT, FS])
        self.fit('FLOOR.L.S',  [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 11]],        [FD, FS])
        self.fit('FLOOR.L.D',  [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 11]],        [FD, FS])
        self.fit('FLOOR.W.S',  [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 15]],        [FD, FS])
        self.fit('FLOOR.W.D',  [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 15]],        [FD, FS])
        self.fit('LDC1',       [[OPCODE, 53], BASE, FT, IMMEDIATE],                       [FT, IMMEDIATE, BASE])
        self.fit('LWC1',       [[OPCODE, 49], BASE, FT, IMMEDIATE],                       [FT, IMMEDIATE, BASE])
        self.fit('MFC1',       [[OPCODE, 17], 5, RT, FS, 11],                             [RT, FS])
        self.fit('MOV.S',      [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 6]],         [FD, FS])
        self.fit('MOV.D',      [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 6]],         [FD, FS])
        self.fit('MTC1',       [[OPCODE, 17], [EX_OPCODE, 4], RT, FS, 11],                [RT, FS])
        self.fit('MUL.S',      [[OPCODE, 17], [FMT, 16], FT, FS, FD, [OPCODE, 2]],        [FD, FS, FT])
        self.fit('MUL.D',      [[OPCODE, 17], [FMT, 17], FT, FS, FD, [OPCODE, 2]],        [FD, FS, FT])
        self.fit('NEG.S',      [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 7]],         [FD, FS])
        self.fit('NEG.D',      [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 7]],         [FD, FS])
        self.fit('ROUND.L.S',  [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 8]],         [FD, FS])
        self.fit('ROUND.L.D',  [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 8]],         [FD, FS])
        self.fit('ROUND.W.S',  [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 12]],        [FD, FS])
        self.fit('ROUND.W.D',  [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 12]],        [FD, FS])
        self.fit('SDC1',       [[OPCODE, 61], BASE, FT, IMMEDIATE],                       [FT, IMMEDIATE, BASE])
        self.fit('SQRT.S',     [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 4]],         [FD, FS])
        self.fit('SQRT.D',     [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 4]],         [FD, FS])
        self.fit('SUB.S',      [[OPCODE, 17], [FMT, 16], FT, FS, FD, [OPCODE, 1]],        [FD, FS, FT])
        self.fit('SUB.D',      [[OPCODE, 17], [FMT, 17], FT, FS, FD, [OPCODE, 1]],        [FD, FS, FT])
        self.fit('SWC1',       [[OPCODE, 57], BASE, FT, IMMEDIATE],                       [FT, IMMEDIATE, BASE])
        self.fit('TRUNC.L.S',  [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 9]],         [FD, FS])
        self.fit('TRUNC.L.D',  [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 9]],         [FD, FS])
        self.fit('TRUNC.W.S',  [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 13]],        [FD, FS])
        self.fit('TRUNC.W.D',  [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 13]],        [FD, FS])

    def fit(self, mnemonic, encoding, appearance):
        comparable_bits = ''
        identifying_bits = ''
        appearance_bit_correspondence = {}
        for i in encoding:
            segment_type = type_of(i)
            if segment_type == 'str':
                comparable_to_append = '0' * LENGTHS[i]
                identifying_to_append = '0' * LENGTHS[i]
                bits_addressed = len(comparable_bits)
                shift_amount = 32 - (LENGTHS[i] + bits_addressed)
                appearance_bit_correspondence[i] = [
                    int('0b' + ('0' * bits_addressed) + ('1' * LENGTHS[i]) + ('0' * shift_amount), 2),
                    shift_amount
                ]
            elif segment_type == 'int':
                comparable_to_append = '1' * i
                identifying_to_append = '0' * i
            else:
                comparable_to_append = '1' * LENGTHS[i[0]]
                identifying_to_append = extend_zeroes(bin(i[1])[2:], LENGTHS[i[0]])
            comparable_bits += comparable_to_append
            identifying_bits += identifying_to_append
        appearance = [[i, appearance_bit_correspondence[i]] for i in appearance]
        self.comparable_bits.append(int('0b' + comparable_bits, 2))
        self.identifying_bits.append(int('0b' + identifying_bits, 2))
        # self.appearance_bit_correspondences.append(appearance_bit_correspondence)
        self.mnemonics.append(mnemonic)
        self.encodes.append(encoding)
        self.appearances.append(appearance)
        self.amount += 1

    def decode(self, int_word, index):
        if int_word == 0:
            return 'NOP'
        identity = None
        # Bitwise & the code with each instruction's comparable bits and check it against the corresponding identifying bits
        for i in range(self.amount):
            if int_word & self.comparable_bits[i] == self.identifying_bits[i]:
                identity = i
                break
        if identity == None:
            return 'UNKNOWN/NOT AN INSTRUCTION'
        parameters = ''
        mnemonic = self.mnemonics[identity]
        index += 1  # Jump/branch to offsets/addresses are calculated based on the address of the delay slot (following word)
        for i in self.appearances[identity]:
            param_name = i[0]
            param_type = CODE_TYPES[param_name]
            bit_correspondence = i[1][0]
            bit_shift = i[1][1]
            inner_value = (bit_correspondence & int_word) >> bit_shift
            if param_type == 'HEX':
                is_address = param_name == 'ADDRESS'
                is_offset = param_name == 'OFFSET'
                if is_offset:
                    # Value needs 16 bit signing, then adding to the base address of the offset, then multiplying by 4
                    # These steps transform the inner value into the target address of branch instructions
                    # Branch instructions are the only ones to use offsets in this manner
                    # Load and Store instructions are supposed to use offsets instead of immediate values
                    #   but they were changed to immediate so they are treated appropriately, and so these
                    #   transformations won't apply to them.
                    inner_value = sign_16_bit_value(inner_value) + index
                    if inner_value <= 0:
                        # These are instructions where the target address is under 0; definitely some storage bytes that aren't an instruction
                        # It causes a problem when trying to re-encode the instruction as the user scrolls into it's view, so return unknown
                        return 'UNKNOWN/NOT AN INSTRUCTION'
                    inner_value <<= 2
                if is_address:
                    # Add the current 268mb alignment to the value to properly decode
                    inner_value += (index // ADDRESS_ALIGNMENT) * ADDRESS_ALIGNMENT
                    inner_value <<= 2
                if self.game_address_mode and (is_address or is_offset):
                    inner_value += self.game_offset
                decode_text = self.immediate_identifier + extend_zeroes(hexi(inner_value), HEX_EXTEND[param_name])
            else: # Will be a register
                decode_text = REGISTERS[param_type][inner_value]
                if param_name == 'BASE':
                    decode_text = '(' + decode_text + ')'
            if parameters != '':
                if param_name == 'BASE':
                    decode_text = ' ' + decode_text
                else:
                    decode_text = ', ' + decode_text
            else:
                decode_text = ' ' + decode_text
            parameters += decode_text
        return mnemonic + parameters

    def encode(self, string, index):
        if string in ['', 'NOP']:
            return 0
        punc = string.find(' ')
        opcode = string[:punc]
        string = string[punc + 1:]
        try:
            id = self.mnemonics.index(opcode)
        except:
            # Syntax error - no such mnemonic
            return -1
        str_parameters = []
        index += 1  # Jump/branch to offsets/addresses are calculated based on the address of the delay slot (following word)
        try:
            while len(string):
                punc = string.find(', ')
                if punc < 0:
                    punc = string.find(' (')
                if punc < 0:
                    punc = string.find(')')
                if punc < 0:
                    punc = len(string)
                str_parameters.append(string[:punc])
                string = string[punc + 2:]
                if len(str_parameters) == len(self.appearances[id]):
                    break
            int_result = self.identifying_bits[id]
            for i in range(len(self.appearances[id])):
                param = str_parameters[i]
                if param[0] == self.immediate_identifier:
                    param = deci(param[1:])
                    is_address = self.appearances[id][i][0] == 'ADDRESS'
                    is_offset = self.appearances[id][i][0] == 'OFFSET'
                    if self.game_address_mode and (is_address or is_offset):
                        param -= self.game_offset
                    if is_address:
                        param >>= 2
                        if index // ADDRESS_ALIGNMENT != param // ADDRESS_ALIGNMENT:
                            # Immediate out of bounds error - not within the 268mb aligned region
                            return -3
                        param %= ADDRESS_ALIGNMENT
                    elif is_offset:
                        param >>= 2
                        param = unsign_16_bit_value(param - index)
                        if param > MAXIMUM_VALUES['OFFSET'] or param < 0:
                            # Immediate out of bounds error
                            return -3
                    else:
                        if self.appearances[id][i][0] not in MAXIMUM_VALUES.keys():
                            # Syntax error - used immediate where a parameter should be a register
                            return -2
                        if param // 4 > MAXIMUM_VALUES[self.appearances[id][i][0]] or param < 0:
                            # Immediate out of bounds error
                            return -3
                else:
                    param = REGISTERS_ENCODE[param]
                int_result += param << self.appearances[id][i][1][1]
        except:
            # Syntax error
            return -2
        return int_result

    def split_and_store_bytes(self, int, index):
        # todo: probably a better way to do this
        ints = get_8_bit_ints_from_32_bit_int(int)
        for i in range(4):
            self.hack_file[(index << 2) + i] = ints[i]

