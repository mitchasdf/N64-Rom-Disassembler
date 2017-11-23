
from function_defs import deci, hexi, ints_of_4_byte_aligned_region, \
    string_to_dict, extend_zeroes, sign_16_bit_value, unsign_16_bit_value, \
    get_8_bit_ints_from_32_bit_int, int_of_4_byte_aligned_region, key_in_dict
from pickle import dump, load
from os.path import exists
from os import remove
from tkinter.simpledialog import messagebox
import tkinter



OPCODE = 'OPCODE'  # For identifying instruction (6 bits)
EX_OPCODE = 'EX_OPCODE'  # For identifying instruction - compared with after all other named bit segments (5 bits)
OFFSET = 'OFFSET'  # Static values in which are represented as numbers  -- Numerical Parameter (16 bits)
VOFFSET = 'VOFFSET' # Like OFFSET, but for vector instructions (6 bits)
ADDRESS = 'ADDRESS'  # Static values in which are represented as numbers  -- Numerical Parameter (26 bits)
IMMEDIATE = 'IMMEDIATE'  # Static values in which are represented as numbers  -- Numerical Parameter (16 bits)
BASE = 'BASE'  # Use as base before offset applies to target address  -- Register Parameter (5 bits)
RT = 'RT'  # General Purpose Register Target  -- Register Parameter (5 bits)
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
CO = 'CO'  # For identifying instruction?
ES = 'ES'  # For identifying instruction
CODE_10 = 'CODE_10'  # Code (10bit)  -- Numerical Parameter
CODE_20 = 'CODE_20'  # Code (20bit)  -- Numerical Parameter
VS = 'VS'
VD = 'VD'
VT = 'VT'
MOD = 'MOD'

ADDRESS_ALIGNMENT = 0x04000000
ADDRESS_ALIGNMENT_4 = 0x10000000

MAXIMUM_VALUES = {  # For Disassembler.encode(): How high the limit will be for immediate types of parameters
    'ADDRESS': 0x3FFFFFF,
    'OFFSET': 0xFFFF,
    'IMMEDIATE': 0xFFFF,
    'CODE_20': 0xFFFFF,
    'CODE_10': 0x3FF,
    'VOFFSET': 0x3F,
    'OP': 0x1F,
    'SA': 0x1F,
    'MOD': 0xF,
}

HEX_EXTEND = {  # For Disassembler.decode(): How many digits are minimally required to represent each numerical value
    'ADDRESS': 8,
    'OFFSET': 8,
    'IMMEDIATE': 4,
    'CODE_20': 5,
    'CODE_10': 3,
    'VOFFSET': 2,
    'OP': 2,
    'MOD': 2,
    'SA': 1
}

CODE_TYPES = {  # For Disassembler.decode(): The instances in which the parameters will translate to
    'ADDRESS': 'HEX',
    'OFFSET': 'HEX',
    'IMMEDIATE': 'HEX',
    'CODE_20': 'HEX',
    'CODE_10': 'HEX',
    'VOFFSET': 'HEX',
    'OP': 'HEX',
    'SA': 'HEX',
    'MOD': 'VHEX',
    'BASE': 'MAIN',
    'RT': 'MAIN',
    'RD': 'MAIN',
    'RS': 'MAIN',
    'FT': 'FLOAT',
    'FD': 'FLOAT',
    'FS': 'FLOAT',
    'VT': 'VECTOR',
    'VD': 'VECTOR',
    'VS': 'VECTOR',
    'CS': 'CP0',
}

LENGTHS = {  # For Disassembler.fit(), how long each segment of data will be, in bits
    'ADDRESS': 26,
    'CODE_20': 20,
    'OFFSET': 16,
    'IMMEDIATE': 16,
    'CODE_10': 10,
    'OPCODE': 6,
    'VOFFSET': 6,
    'EX_OPCODE': 5,
    'BASE': 5,
    'RT': 5,
    'RD': 5,
    'RS': 5,
    'FT': 5,
    'FD': 5,
    'FS': 5,
    'VT': 5,
    'VD': 5,
    'VS': 5,
    'CS': 5,
    'SA': 5,
    'STYPE': 5,
    'FMT': 5,
    'OP': 5,
    'COND': 4,
    'MOD': 4,
    'ES': 2,
    'CO': 1
}

REGISTERS = {
    'MAIN': [
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

    'FLOAT': ['F' + extend_zeroes(str(i), 2) for i in range(32)],

    'CP0': [
        'INDEX',
        'RANDOM',
        'ENTRYLO0',
        'ENTRYLO1',
        'CONTEXT',
        'PAGEMASK',
        'WIRED',
        'RESERVED0',
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
        'RESERVED1',
        'RESERVED2',
        'RESERVED3',
        'RESERVED4',
        'RESERVED5',
        'PERR',
        'CACHEERR',
        'TAGLO',
        'TAGHI',
        'ERROREPC',
        'RESERVED6'
    ],

    'VECTOR': ['V' + extend_zeroes(str(i), 2) for i in range(32)]
}

CODES_USING_ADDRESSES = ['BC1F', 'BC1FL', 'BC1T', 'BC1TL', 'BEQ', 'BEQL', 'BGEZ', 'BGEZAL', 'BGEZALL',
                         'BGEZL', 'BGTZ', 'BGTZL', 'BLEZ', 'BLEZL', 'BLTZ', 'BLTZAL', 'BLTZALL', 'BLTZL',
                         'BNEZ', 'BNEL', 'BNE', 'J', 'JAL']

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
    'F00': 0,
    'F01': 1,
    'F02': 2,
    'F03': 3,
    'F04': 4,
    'F05': 5,
    'F06': 6,
    'F07': 7,
    'F08': 8,
    'F09': 9,
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
    'V00': 0,
    'V01': 1,
    'V02': 2,
    'V03': 3,
    'V04': 4,
    'V05': 5,
    'V06': 6,
    'V07': 7,
    'V08': 8,
    'V09': 9,
    'V10': 10,
    'V11': 11,
    'V12': 12,
    'V13': 13,
    'V14': 14,
    'V15': 15,
    'V16': 16,
    'V17': 17,
    'V18': 18,
    'V19': 19,
    'V20': 20,
    'V21': 21,
    'V22': 22,
    'V23': 23,
    'V24': 24,
    'V25': 25,
    'V26': 26,
    'V27': 27,
    'V28': 28,
    'V29': 29,
    'V30': 30,
    'V31': 31,
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
}

JUMP_INTS = [True if i in [2, 3] else False for i in range(64)]

BRANCH_INTS = [True if i in [1, 4, 5, 6, 7, 20, 21, 22, 23] else False for i in range(64)]

DOCUMENTATION = {
    'LB': 'Load Byte',
    'LBU': 'Load Byte Unsigned',
    'LD': 'Load Double',
    'LDL': 'Load Double Left',
    'LDR': 'Load Double Right',
    'LH': 'Load Half',
    'LHU': 'Load Half Unsigned',
    'LL': 'Load Linked Word',
    'LLD': 'Load Linked Double',
    'LW': 'Load Word',
    'LWL': 'Load Word Left',
    'LWR': 'Load Word Right',
    'LWU': 'Load Word Unsigned',
    'SB': 'Store Byte',
    'SC': 'Store Conditional Word',
    'SCD': 'Store Conditional Double',
    'SD': 'Store Double',
    'SDL': 'Store Double Left',
    'SDR': 'Store Double Right',
    'SH': 'Store Half',
    'SW': 'Store Word',
    'SWL': 'Store Word Left',
    'SWR': 'Store Word Right',
    'SYNC': 'Synchronize Shade Memory',
    'ADD': 'Add Word',
    'ADDI': 'Add Immediate Word',
    'ADDIU': 'Add Immediate Unsigned Word',
    'ADDU': 'Add Unsigned Word',
    'AND': 'Bitwise AND Word',
    'ANDI': 'Bitwise AND Immediate',
    'DADD': 'Double Add',
    'DADDI': 'Double Add Immediate',
    'DADDIU': 'Double Add Immediate Unsigned',
    'DADDU': 'Double Add Unsigned',
    'DDIV': 'Double Divide',
    'DDIVU': 'Double Divide Unsigned',
    'DIV': 'Divide Word',
    'DIVU': 'Divide Word Unsigned',
    'DMULT': 'Multiply Double',
    'DMULTU': 'Multiply Double Unsigned',
    'DSLL': 'Double Shift Left Logical',
    'DSLL32': 'Double Shift Left Logical (+32)',
    'DSLLV': 'Double Shift Left Logical Variable',
    'DSRA': 'Double Shift Right Arithmetic',
    'DSRA32': 'Double Shift right Arithmetic (+32)',
    'DSRAV': 'Double Shift Right Arithmetic Variable',
    'DSRL': 'Double Shift Right Logical',
    'DSRL32': 'Double Shift Right Logical (+32)',
    'DSRLV': 'Double Shift Right Logical Variable',
    'DSUB': 'Double Subtract',
    'DSUBU': 'Double Subtract Unsigned',
    'LUI': 'Load Upper Immediate',
    'MFHI': 'Move From HI Register',
    'MFLO': 'Move From LO Register',
    'MTHI': 'Move To HI Register',
    'MTLO': 'Move To LO Register',
    'MULT': 'Multiply Word',
    'MULTU': 'Multiply Word Unsigned',
    'NOR': 'Bitwise NOR Word',
    'OR': 'Bitwise OR Word',
    'ORI': 'Bitwise OR Immediate Word',
    'SLL': 'Word Shift Left Logical',
    'SLLV': 'Word Shift Left Logical Variable',
    'SLT': 'Set On Less Than',
    'SLTI': 'Set On Less Than Immediate',
    'SLTIU': 'Set On Less Than Immediate Unsigned',
    'SLTU': 'Set On Less Than Unsigned',
    'SRA': 'Word Shift Right Arithmetic',
    'SRAV': 'Word Shift Right Arithmetic Variable',
    'SRL': 'Word Shift Right Logical',
    'SRLV': 'Word Shift Right Logical Variable',
    'SUB': 'Subtract Word',
    'SUBU': 'Subtract Word Unsigned',
    'XOR': 'Bitwise XOR Word',
    'XORI': 'Bitwise XOR Word Immediate',
    'BEQ': 'Branch On Equal',
    'BEQL': 'Branch On Equal Likely',
    'BGEZ': 'Branch On Greater Or Equal To Zero',
    'BGEZAL': 'Branch On Greater Or Equal To Zero And Link',
    'BGEZALL': 'Branch On Greater Or Equal To Zero And Link Likely',
    'BGEZL': 'Branch On Greater Or Equal To Zero Likely',
    'BGTZ': 'Branch On Greater Than Zero',
    'BGTZL': 'Branch On Greater Than Zero Likely',
    'BLEZ': 'Branch On Lesser Or Equal To Zero',
    'BLEZL': 'Branch On Lesser Or Equal To Zero Likely',
    'BLTZ': 'Branch On Less Than Zero',
    'BLTZAL': 'Branch On Less Than Zero And Link',
    'BLTZALL': 'Branch On Less Than Zero And Link Likely',
    'BLTZL': 'Branch On Less Than Zero Likely',
    'BNEZ': 'Branch On Not Equal To Zero',
    'BNEL': 'Branch On Not Equal Likely',
    'BNE': 'Branch On Not Equal',
    'J': 'Jump',
    'JAL': 'Jump And Link',
    'JALR': 'Jump And Link Register',
    'JR': 'Jump Register',
    'BREAK': 'Breakpoint',
    'SYSCALL': 'System Call',
    'TEQ': 'Trap If Equal',
    'TEQI': 'Trap If Equal Immediate',
    'TGE': 'Trap If Greater Or Equal To',
    'TGEI': 'Trap If Greater Or Equal To Immediate',
    'TGEIU': 'Trap If Greater Or Equal To Immediate Unsigned',
    'TGEU': 'Trap If Greater Or Equal To Unsigned',
    'TLT': 'Trap If Less Than',
    'TLTI': 'Trap If Less Than Immediate',
    'TLTIU': 'Trap If Less Than Immediate Unsigned',
    'TLTU': 'Trap If Less Than Unsigned',
    'TNE': 'Trap If Not Equal',
    'TNEI': 'Trap If Not Equal To Immediate',
    'CACHE': 'Cache Operation',
    'DMFC0': 'Double Move From COP0 Register',
    'DMTC0': 'Double Move To Float COP0 Register',
    'ERET': 'Return From Exception',
    'MFC0': 'Move Word From COP0 Register',
    'MTC0': 'Move Word To COP0 Register',
    'TLBP': 'Probe TLB For Matching Entry',
    'TLBR': 'Read Indexed TLB Entry',
    'TLBWI': 'Write Indexed TLB Entry',
    'TLBWR': 'Write Random TLB Entry',
    'C.F.S': 'Float Compare Single False',
    'C.UN.S': 'Float Compare Single Unordered',
    'C.EQ.S': 'Float Compare Single Equal',
    'C.UEQ.S': 'Float Compare Single Unordered or Equal',
    'C.OLT.S': 'Float Compare Single Ordered or Less Than',
    'C.ULT.S': 'Float Compare Single Unordered or Less Than',
    'C.OLE.S': 'Float Compare Single Ordered or Less Than or Equal',
    'C.ULE.S': 'Float Compare Single Unordered or Less Than or Equal',
    'C.SF.S': 'Float Compare Single Signaling False',
    'C.NGLE.S': 'Float Compare Single Not Greater Than or Less Than or Equal',
    'C.SEQ.S': 'Float Compare Single Signaling Equal',
    'C.NGL.S': 'Float Compare Single Not Greater Than or Less Than',
    'C.LT.S': 'Float Compare Single Less Than',
    'C.NGE.S': 'Float Compare Single Not Greater Than or Equal',
    'C.LE.S': 'Float Compare Single Less Than or Equal',
    'C.NGT.S': 'Float Compare Single Not Greater Than',
    'C.F.D': 'Float Compare Double False',
    'C.UN.D': 'Float Compare Double Unordered',
    'C.EQ.D': 'Float Compare Double Equal',
    'C.UEQ.D': 'Float Compare Double Unordered or Equal',
    'C.OLT.D': 'Float Compare Double Ordered or Less Than',
    'C.ULT.D': 'Float Compare Double Unordered or Less Than',
    'C.OLE.D': 'Float Compare Double Ordered or Less Than or Equal',
    'C.ULE.D': 'Float Compare Double Unordered or Less Than or Equal',
    'C.SF.D': 'Float Compare Double Signaling False',
    'C.NGLE.D': 'Float Compare Double Not Greater Than or Less Than or Equal',
    'C.SEQ.D': 'Float Compare Double Signaling Equal',
    'C.NGL.D': 'Float Compare Double Not Greater Than or Less Than',
    'C.LT.D': 'Float Compare Double Less Than',
    'C.NGE.D': 'Float Compare Double Not Greater Than or Equal',
    'C.LE.D': 'Float Compare Double Less Than or Equal',
    'C.NGT.D': 'Float Compare Double Not Greater Than',
    'ABS.S': 'Float Absolute Word',
    'ABS.D': 'Float Absolute Double',
    'ADD.S': 'Float Add Word',
    'ADD.D': 'Float Add Double',
    'BC1F': 'Float Branch On False',
    'BC1FL': 'Float Branch On False Likely',
    'BC1T': 'Float Branch On True',
    'BC1TL': 'Float Branch On True Likely',
    'CEIL.L.S': 'Float Ceil Single Convert to Long Fixed',
    'CEIL.L.D': 'Float Ceil Double Convert to Long Fixed',
    'CEIL.W.S': 'Float Ceil Single Convert to Word Fixed',
    'CEIL.W.D': 'Float Ceil Double Convert to Word Fixed',
    'CFC1': 'Move Control Word from Float',
    'CTC1': 'Move Control Word to Float',
    'CVT.D.S': 'Float Convert Single to Double Float',
    'CVT.D.W': 'Float Convert Word to Double Float',
    'CVT.D.L': 'Float Convert Long to Double Float',
    'CVT.L.S': 'Float Convert Single to Long Fixed',
    'CVT.L.D': 'Float Convert Double to Long Fixed',
    'CVT.S.D': 'Float Convert Double to Single Float',
    'CVT.S.W': 'Float Convert Word to Single Float',
    'CVT.S.L': 'Float Convert Long to Single Float',
    'CVT.W.S': 'Float Convert Single to Word Fixed',
    'CVT.W.D': 'Float Convert Word to Word Fixed',
    'DIV.S': 'Float Divide Single',
    'DIV.D': 'Float Divide Double',
    'DMFC1': 'Double Move From Float',
    'DMTC1': 'Double Move To Float',
    'FLOOR.L.S': 'Float Floor Single Convert to Long Fixed',
    'FLOOR.L.D': 'Float Floor Double Convert to Long Fixed',
    'FLOOR.W.S': 'Float Floor Single Convert to Word Fixed',
    'FLOOR.W.D': 'Float Floor Double Convert to Word Fixed',
    'LDC1': 'Load Double to Float',
    'LWC1': 'Load Word to Float',
    'MFC1': 'Move Word from Float',
    'MOV.S': 'Move Float Single',
    'MOV.D': 'Move Float Double',
    'MTC1': 'Move Word to Float',
    'MUL.S': 'Float Multiply Single',
    'MUL.D': 'Float Multiply Double',
    'NEG.S': 'Float Negate Single',
    'NEG.D': 'Float Negate Double',
    'ROUND.L.S': 'Float Round Single to Long Fixed',
    'ROUND.L.D': 'Float Round Double to Long Fixed',
    'ROUND.W.S': 'Float Round Single to Word Fixed',
    'ROUND.W.D': 'Float Round Double to Word Fixed',
    'SDC1': 'Store Double from Float',
    'SQRT.S': 'Float Square Root Single',
    'SQRT.D': 'Float Square Rood Double',
    'SUB.S': 'Float Subtract Single',
    'SUB.D': 'Float Subtract Double',
    'SWC1': 'Store Word from Float',
    'TRUNC.L.S': 'Float Truncate Single to Long Fixed',
    'TRUNC.L.D': 'Float Truncate Double to Long Fixed',
    'TRUNC.W.S': 'Float Truncate Single to Word Fixed',
    'TRUNC.W.D': 'Float Truncate Double to Word Fixed',
}


class Disassembler:
    def __init__(self, base_file_path, hacked_file_path, window, status_bar):
        def open_rom(file_path):
            if self.base_file and not exists(file_path):
                with open(file_path, 'wb') as hack_file:
                    hack_file.write(self.base_file)
            with open(file_path, 'rb') as file:
                file_data = bytearray(file.read())
            part_at = file_path.rfind('\\') + 1
            folder = file_path[:part_at]
            file_name = file_path[part_at:]
            rom_validation = file_data[0:4].hex()
            rom_type = ''
            if rom_validation == '37804012':
                rom_type = 'byte-swap'
                if not self.base_file:
                    status_bar.set('{} is byte-swapped, swapping now...'.format(file_name))
                    window.update_idletasks()
                # Rom is byte-swapped, so swap it to the right order
                i = 0
                while i < len(file_data):
                    byte_2, byte_4 = file_data[i+1], file_data[i+3]
                    file_data[i+1] = file_data[i]
                    file_data[i] = byte_2
                    file_data[i+3] = file_data[i+2]
                    file_data[i+2] = byte_4
                    i += 4
            elif rom_validation == '40123780':
                rom_type = 'little-end'
                if not self.base_file:
                    status_bar.set('{} is in little-endian, reversing now...'.format(file_name))
                    window.update_idletasks()
                # Rom is in little-endian, swap to big
                i = 0
                while i < len(file_data):
                    byte_1, byte_2 = file_data[i], file_data[i+1]
                    file_data[i] = file_data[i+3]
                    file_data[i+1] = file_data[i+2]
                    file_data[i+2] = byte_2
                    file_data[i+3] = byte_1
                    i += 4
            elif rom_validation != '80371240':
                raise Exception('"{}" Not a rom file'.format(file_path))
            elif self.base_file:
                if len(self.base_file) != len(file_data):
                    raise Exception('These roms are different sizes. If you have already had your hacked rom extended, '
                                    'then you can "start a new hacked rom" and pick your extended rom as the base file.')
            return folder, file_name, file_data, rom_type

        self.base_file = bytearray()
        self.base_folder, self.base_file_name, self.base_file, base_type = open_rom(base_file_path)
        self.hack_folder, self.hack_file_name, self.hack_file, hack_type = open_rom(hacked_file_path)

        # Otherwise it takes a long time to re-order bytes every time the roms are loaded
        if base_type in ['byte-swap', 'little-end']:
            dot = self.base_file_name.rfind('.')
            self.base_file_name = self.base_file_name[:dot] + ' (byte-reordered backup)' + \
                self.base_file_name[dot:]
            with open(self.base_folder + self.base_file_name, 'wb') as file:
                file.write(self.base_file)
            status_bar.set('"{}" has been created next to the original.'.format(self.base_file_name))

        if hack_type in ['byte-swap', 'little-end']:
            with open(self.hack_folder + self.hack_file_name, 'wb') as file:
                file.write(self.hack_file)

        self.header_items = {
            # Section labeled   [data_start, data_end (not inclusive)]
            'Rom Validate':     [0x0000, 0x0004],
            'Clock Rate':       [0x0004, 0x0008],
            'Game Offset':      [0x0008, 0x000C],
            'Release':          [0x000C, 0x0010],
            'CRC1':             [0x0010, 0x0014],
            'CRC2':             [0x0014, 0x0018],
            'Rom Name':         [0x0020, 0x0034],
            'Manufacturer ID':   0x003B,
            'Cartridge ID':     [0x003C, 0x003E],
            'Country Code':     [0x003E, 0x0040],
            'Boot Code':        [0x0040, 0x1000],
            'Game Code': 0x1000
        }

        self.comments_file = '{} comments.txt'.format(self.hack_folder + self.hack_file_name)
        self.comments = {}
        self.file_length = len(self.base_file)
        def fresh_comments():
            # Start new comments off with some header labels
            for i in self.header_items.keys():
                if isinstance(self.header_items[i], list):
                    start = self.header_items[i][0]
                    end = self.header_items[i][1]
                    comments_location = '{}'.format((start >> 2))
                    extra_append = ' ({} - {})'.format(extend_zeroes(hexi(start), 4), extend_zeroes(hexi(end - 1), 4))
                else:
                    extra_append = ' ({})'.format(extend_zeroes(hexi(self.header_items[i]), 4))
                    comments_location = '{}'.format((self.header_items[i] >> 2))
                already_data = comments_location in self.comments.keys()
                i += extra_append
                if already_data:
                    self.comments[comments_location] = self.comments[comments_location] + ' | ' + i
                else:
                    self.comments[comments_location] = i
            end_place = str((self.file_length >> 2) - 1)
            self.comments[end_place] = 'End of file'

        if not exists(self.comments_file):
            fresh_comments()
        else:
            # Load the existing comments
            try:
                with open(self.comments_file, 'r') as file:
                    file_contents = file.read()
                if not file_contents:
                    fresh_comments()
                else:
                    self.comments = string_to_dict(file_contents)
            except Exception as e:
                err_tk = tkinter.Tk()
                err_tk.withdraw()
                messagebox._show('Error', 'The comments file cannot be loaded - so this rom cannot be loaded until this is fixed.\n'
                                 'This is only necessary because if we went ahead and loaded, your comments file would'
                                 ' be erased and overwritten with fresh data.')
                raise Exception('"{}"\n\n'.format(self.comments_file) + str(e))

        # Display the rom name in a user-readable format
        segment = self.hack_file[self.header_items['Rom Name'][0]: self.header_items['Rom Name'][1]]
        self.comments['9'] = segment.decode()

        # Save the game offset
        segment = self.hack_file[self.header_items['Game Offset'][0]: self.header_items['Game Offset'][1]]
        self.game_offset = int.from_bytes(segment, byteorder='big', signed=False) - 0x1000

        self.game_address_mode = False
        self.immediate_identifier = '$'

        self.encodes = {}
        self.appearances = {}
        self.identifying_bits = {}
        self.opcode_matrix = []
        self.jumps_to = {}
        self.branches_to = {}
        self.jumps_file = '{}{} jumps.data'.format(self.hack_folder, self.hack_file_name)
        self.documentation = {}

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


            The fitment parameters only "look" like the MIPS documentation, but a few things have been changed to 
               better accommodate the encoding and decoding process.
            
            For performance and 100% reliable decoding process, all Pseudo Opcodes are omitted.
        '''

        # Missing 0th to 5th bit OPCODES:  19, 28, 29, 30, 31, 54, 58, 59, 62

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
        self.fit('SYNC',  [[OPCODE, 0], 20, [OPCODE, 15]],      [])

        # Arithmetic Instructions
        self.fit('ADD',     [[OPCODE, 0], RS, RT, RD, 5, [OPCODE, 32]],  [RD, RS, RT])
        self.fit('ADDI',    [[OPCODE, 8], RS, RT, IMMEDIATE],            [RT, RS, IMMEDIATE])
        self.fit('ADDIU',   [[OPCODE, 9], RS, RT, IMMEDIATE],            [RT, RS, IMMEDIATE])
        self.fit('ADDU',    [[OPCODE, 0], RS, RT, RD, 5, [OPCODE, 33]],  [RD, RS, RT])
        self.fit('AND',     [[OPCODE, 0], RS, RT, RD, 5, [OPCODE, 36]],  [RD, RS, RT])
        self.fit('ANDI',    [[OPCODE, 12], RS, RT, IMMEDIATE],           [RT, RS, IMMEDIATE])
        self.fit('DADD',    [[OPCODE, 0], RS, RT, RD, 5, [OPCODE, 44]],  [RD, RS, RT])
        self.fit('DADDI',   [[OPCODE, 24], RS, RT, IMMEDIATE],           [RT, RS, IMMEDIATE])
        self.fit('DADDIU',  [[OPCODE, 25], RS, RT, IMMEDIATE],           [RT, RS, IMMEDIATE])
        self.fit('DADDU',   [[OPCODE, 0], RS, RT, RD, 5, [OPCODE, 45]],  [RD, RS, RT])
        self.fit('DDIV',    [[OPCODE, 0], RS, RT, 10, [OPCODE, 30]],     [RS, RT])
        self.fit('DDIVU',   [[OPCODE, 0], RS, RT, 10, [OPCODE, 31]],     [RS, RT])
        self.fit('DIV',     [[OPCODE, 0], RS, RT, 10, [OPCODE, 26]],     [RS, RT])
        self.fit('DIVU',    [[OPCODE, 0], RS, RT, 10, [OPCODE, 27]],     [RS, RT])
        self.fit('DMULT',   [[OPCODE, 0], RS, RT, 10, [OPCODE, 28]],     [RS, RT])
        self.fit('DMULTU',  [[OPCODE, 0], RS, RT, 10, [OPCODE, 29]],     [RS, RT])
        self.fit('DSLL',    [[OPCODE, 0], 5, RT, RD, SA, [OPCODE, 56]],  [RD, RT, SA])
        self.fit('DSLL32',  [[OPCODE, 0], 5, RT, RD, SA, [OPCODE, 60]],  [RD, RT, SA])
        self.fit('DSLLV',   [[OPCODE, 0], RS, RT, RD, 5, [OPCODE, 20]],  [RD, RT, RS])
        self.fit('DSRA',    [[OPCODE, 0], 5, RT, RD, SA, [OPCODE, 59]],  [RD, RT, SA])
        self.fit('DSRA32',  [[OPCODE, 0], 5, RT, RD, SA, [OPCODE, 63]],  [RD, RT, SA])
        self.fit('DSRAV',   [[OPCODE, 0], RS, RT, RD, 5, [OPCODE, 23]],  [RD, RT, RS])
        self.fit('DSRL',    [[OPCODE, 0], 5, RT, RD, SA, [OPCODE, 58]],  [RD, RT, SA])
        self.fit('DSRL32',  [[OPCODE, 0], 5, RT, RD, SA, [OPCODE, 62]],  [RD, RT, SA])
        self.fit('DSRLV',   [[OPCODE, 0], RS, RT, RD, 5, [OPCODE, 22]],  [RD, RT, RS])
        self.fit('DSUB',    [[OPCODE, 0], RS, RT, RD, 5, [OPCODE, 46]],  [RD, RS, RT])
        self.fit('DSUBU',   [[OPCODE, 0], RS, RT, RD, 5, [OPCODE, 47]],  [RD, RS, RT])
        self.fit('LUI',     [[OPCODE, 15], 5, RT, IMMEDIATE],            [RT, IMMEDIATE])
        self.fit('MFHI',    [[OPCODE, 0], 5, 5, RD, 5, [OPCODE, 16]],    [RD])
        self.fit('MFLO',    [[OPCODE, 0], 5, 5, RD, 5, [OPCODE, 18]],    [RD])
        self.fit('MTHI',    [[OPCODE, 0], RS, 15, [OPCODE, 17]],         [RS])
        self.fit('MTLO',    [[OPCODE, 0], RS, 15, [OPCODE, 19]],         [RS])
        self.fit('MULT',    [[OPCODE, 0], RS, RT, 10, [OPCODE, 24]],     [RS, RT])
        self.fit('MULTU',   [[OPCODE, 0], RS, RT, 10, [OPCODE, 25]],     [RS, RT])
        self.fit('NOR',     [[OPCODE, 0], RS, RT, RD, 5, [OPCODE, 39]],  [RD, RS, RT])
        self.fit('OR',      [[OPCODE, 0], RS, RT, RD, 5, [OPCODE, 37]],  [RD, RS, RT])
        self.fit('ORI',     [[OPCODE, 13], RS, RT, IMMEDIATE],           [RT, RS, IMMEDIATE])
        self.fit('SLL',     [[OPCODE, 0], 5, RT, RD, SA, [OPCODE, 0]],   [RD, RT, SA])
        self.fit('SLLV',    [[OPCODE, 0], RS, RT, RD, 5, [OPCODE, 4]],   [RD, RT, RS])
        self.fit('SLT',     [[OPCODE, 0], RS, RT, RD, 5, [OPCODE, 42]],  [RD, RS, RT])
        self.fit('SLTI',    [[OPCODE, 10], RS, RT, IMMEDIATE],           [RT, RS, IMMEDIATE])
        self.fit('SLTIU',   [[OPCODE, 11], RS, RT, IMMEDIATE],           [RT, RS, IMMEDIATE])
        self.fit('SLTU',    [[OPCODE, 0], RS, RT, RD, 5, [OPCODE, 43]],  [RD, RS, RT])
        self.fit('SRA',     [[OPCODE, 0], 5, RT, RD, SA, [OPCODE, 3]],   [RD, RT, SA])
        self.fit('SRAV',    [[OPCODE, 0], RS, RT, RD, 5, [OPCODE, 7]],   [RD, RT, RS])
        self.fit('SRL',     [[OPCODE, 0], 5, RT, RD, SA, [OPCODE, 2]],   [RD, RT, SA])
        self.fit('SRLV',    [[OPCODE, 0], RS, RT, RD, 5, [OPCODE, 6]],   [RD, RT, RS])
        self.fit('SUB',     [[OPCODE, 0], RS, RT, RD, 5, [OPCODE, 34]],  [RD, RS, RT])
        self.fit('SUBU',    [[OPCODE, 0], RS, RT, RD, 5, [OPCODE, 35]],  [RD, RS, RT])
        self.fit('XOR',     [[OPCODE, 0], RS, RT, RD, 5, [OPCODE, 38]],  [RD, RS, RT])
        self.fit('XORI',    [[OPCODE, 14], RS, RT, IMMEDIATE],           [RT, RS, IMMEDIATE])

        # Jump and Branch Instructions
        self.fit('BEQ',      [[OPCODE, 4], RS, RT, OFFSET],             [RS, RT, OFFSET])
        self.fit('BEQL',     [[OPCODE, 20], RS, RT, OFFSET],            [RS, RT, OFFSET])
        self.fit('BGEZ',     [[OPCODE, 1], RS, [FMT, 1], OFFSET],       [RS, OFFSET])
        self.fit('BGEZAL',   [[OPCODE, 1], RS, [FMT, 17], OFFSET],      [RS, OFFSET])
        self.fit('BGEZALL',  [[OPCODE, 1], RS, [FMT, 19], OFFSET],      [RS, OFFSET])
        self.fit('BGEZL',    [[OPCODE, 1], RS, [FMT, 3], OFFSET],       [RS, OFFSET])
        self.fit('BGTZ',     [[OPCODE, 7], RS, 5, OFFSET],              [RS, OFFSET])
        self.fit('BGTZL',    [[OPCODE, 23], RS, 5, OFFSET],             [RS, OFFSET])
        self.fit('BLEZ',     [[OPCODE, 6], RS, 5, OFFSET],              [RS, OFFSET])
        self.fit('BLEZL',    [[OPCODE, 22], RS, 5, OFFSET],             [RS, OFFSET])
        self.fit('BLTZ',     [[OPCODE, 1], RS, 5, OFFSET],              [RS, OFFSET])
        self.fit('BLTZAL',   [[OPCODE, 1], RS, [FMT, 16], OFFSET],      [RS, OFFSET])
        self.fit('BLTZALL',  [[OPCODE, 1], RS, [FMT, 18], OFFSET],      [RS, OFFSET])
        self.fit('BLTZL',    [[OPCODE, 1], RS, [FMT, 2], OFFSET],       [RS, OFFSET])
        self.fit('BNEL',     [[OPCODE, 21], RS, RT, OFFSET],            [RS, RT, OFFSET])
        self.fit('BNE',      [[OPCODE, 5], RS, RT, OFFSET],             [RS, RT, OFFSET])
        self.fit('J',        [[OPCODE, 2], ADDRESS],                    [ADDRESS])
        self.fit('JAL',      [[OPCODE, 3], ADDRESS],                    [ADDRESS])
        self.fit('JALR',     [[OPCODE, 0], RS, 5, RD, 5, [OPCODE, 9]],  [RD, RS])
        self.fit('JR',       [[OPCODE, 0], RS, 15, [OPCODE, 8]],        [RS])

        # Special Instructions
        self.fit('BREAK',    [[OPCODE, 0], CODE_20, [OPCODE, 13]],  [CODE_20])
        self.fit('SYSCALL',  [[OPCODE, 0], CODE_20, [OPCODE, 12]],  [CODE_20])

        # Exception Instructions
        self.fit('TEQ',    [[OPCODE, 0], RS, RT, CODE_10, [OPCODE, 52]],  [CODE_10, RS, RT])
        self.fit('TEQI',   [[OPCODE, 1], RS, [FMT, 12], IMMEDIATE],       [RS, IMMEDIATE])
        self.fit('TGE',    [[OPCODE, 0], RS, RT, CODE_10, [OPCODE, 48]],  [CODE_10, RS, RT])
        self.fit('TGEI',   [[OPCODE, 1], RS, [FMT, 8], IMMEDIATE],        [RS, IMMEDIATE])
        self.fit('TGEIU',  [[OPCODE, 1], RS, [FMT, 9], IMMEDIATE],        [RS, IMMEDIATE])
        self.fit('TGEU',   [[OPCODE, 0], RS, RT, CODE_10, [OPCODE, 49]],  [CODE_10, RS, RT])
        self.fit('TLT',    [[OPCODE, 0], RS, RT, CODE_10, [OPCODE, 50]],  [CODE_10, RS, RT])
        self.fit('TLTI',   [[OPCODE, 1], RS, [FMT, 10], IMMEDIATE],       [RS, IMMEDIATE])
        self.fit('TLTIU',  [[OPCODE, 1], RS, [FMT, 11], IMMEDIATE],       [RS, IMMEDIATE])
        self.fit('TLTU',   [[OPCODE, 0], RS, RT, CODE_10, [OPCODE, 51]],  [CODE_10, RS, RT])
        self.fit('TNE',    [[OPCODE, 0], RS, RT, CODE_10, [OPCODE, 54]],  [CODE_10, RS, RT])
        self.fit('TNEI',   [[OPCODE, 1], RS, [FMT, 14], IMMEDIATE],       [RS, IMMEDIATE])

        # System Control Processor (COP0) Instructions
        self.fit('CACHE',  [[OPCODE, 47], BASE, OP, IMMEDIATE],                     [OP, IMMEDIATE, BASE])
        self.fit('DMFC0',  [[OPCODE, 16], [EX_OPCODE, 1], RT, CS, 5, [OPCODE, 0]],  [RT, CS])
        self.fit('DMTC0',  [[OPCODE, 16], [EX_OPCODE, 5], RT, CS, 5, [OPCODE, 0]],  [RT, CS])
        self.fit('ERET',   [[OPCODE, 16], CO, 19, [OPCODE, 24]],                    [])
        self.fit('MFC0',   [[OPCODE, 16], [EX_OPCODE, 0], RT, CS, 5, [OPCODE, 0]],  [RT, CS])
        self.fit('MTC0',   [[OPCODE, 16], [EX_OPCODE, 4], RT, CS, 5, [OPCODE, 0]],  [RT, CS])
        self.fit('TLBP',   [[OPCODE, 16], CO, 19, [OPCODE, 8]],                     [])
        self.fit('TLBR',   [[OPCODE, 16], CO, 19, [OPCODE, 1]],                     [])
        self.fit('TLBWI',  [[OPCODE, 16], CO, 19, [OPCODE, 2]],                     [])
        self.fit('TLBWR',  [[OPCODE, 16], CO, 19, [OPCODE, 6]],                     [])

        # Floating-point Unit (COP1) Instructions
        # These, instead of having OPCODE as the final segment, should be [ES, 3], [COND, (0 to 15 for each code)]
        # But changing it to what I have optimises it slightly
        self.fit('C.F.S',     [[OPCODE, 17], [FMT, 16], FT, FS, 5, [OPCODE, 48]],  [FS, FT])
        self.fit('C.UN.S',    [[OPCODE, 17], [FMT, 16], FT, FS, 5, [OPCODE, 49]],  [FS, FT])
        self.fit('C.EQ.S',    [[OPCODE, 17], [FMT, 16], FT, FS, 5, [OPCODE, 50]],  [FS, FT])
        self.fit('C.UEQ.S',   [[OPCODE, 17], [FMT, 16], FT, FS, 5, [OPCODE, 51]],  [FS, FT])
        self.fit('C.OLT.S',   [[OPCODE, 17], [FMT, 16], FT, FS, 5, [OPCODE, 52]],  [FS, FT])
        self.fit('C.ULT.S',   [[OPCODE, 17], [FMT, 16], FT, FS, 5, [OPCODE, 53]],  [FS, FT])
        self.fit('C.OLE.S',   [[OPCODE, 17], [FMT, 16], FT, FS, 5, [OPCODE, 54]],  [FS, FT])
        self.fit('C.ULE.S',   [[OPCODE, 17], [FMT, 16], FT, FS, 5, [OPCODE, 55]],  [FS, FT])
        self.fit('C.SF.S',    [[OPCODE, 17], [FMT, 16], FT, FS, 5, [OPCODE, 56]],  [FS, FT])
        self.fit('C.NGLE.S',  [[OPCODE, 17], [FMT, 16], FT, FS, 5, [OPCODE, 57]],  [FS, FT])
        self.fit('C.SEQ.S',   [[OPCODE, 17], [FMT, 16], FT, FS, 5, [OPCODE, 58]],  [FS, FT])
        self.fit('C.NGL.S',   [[OPCODE, 17], [FMT, 16], FT, FS, 5, [OPCODE, 59]],  [FS, FT])
        self.fit('C.LT.S',    [[OPCODE, 17], [FMT, 16], FT, FS, 5, [OPCODE, 60]],  [FS, FT])
        self.fit('C.NGE.S',   [[OPCODE, 17], [FMT, 16], FT, FS, 5, [OPCODE, 61]],  [FS, FT])
        self.fit('C.LE.S',    [[OPCODE, 17], [FMT, 16], FT, FS, 5, [OPCODE, 62]],  [FS, FT])
        self.fit('C.NGT.S',   [[OPCODE, 17], [FMT, 16], FT, FS, 5, [OPCODE, 63]],  [FS, FT])
        self.fit('C.F.D',     [[OPCODE, 17], [FMT, 17], FT, FS, 5, [OPCODE, 48]],  [FS, FT])
        self.fit('C.UN.D',    [[OPCODE, 17], [FMT, 17], FT, FS, 5, [OPCODE, 49]],  [FS, FT])
        self.fit('C.EQ.D',    [[OPCODE, 17], [FMT, 17], FT, FS, 5, [OPCODE, 50]],  [FS, FT])
        self.fit('C.UEQ.D',   [[OPCODE, 17], [FMT, 17], FT, FS, 5, [OPCODE, 51]],  [FS, FT])
        self.fit('C.OLT.D',   [[OPCODE, 17], [FMT, 17], FT, FS, 5, [OPCODE, 52]],  [FS, FT])
        self.fit('C.ULT.D',   [[OPCODE, 17], [FMT, 17], FT, FS, 5, [OPCODE, 53]],  [FS, FT])
        self.fit('C.OLE.D',   [[OPCODE, 17], [FMT, 17], FT, FS, 5, [OPCODE, 54]],  [FS, FT])
        self.fit('C.ULE.D',   [[OPCODE, 17], [FMT, 17], FT, FS, 5, [OPCODE, 55]],  [FS, FT])
        self.fit('C.SF.D',    [[OPCODE, 17], [FMT, 17], FT, FS, 5, [OPCODE, 56]],  [FS, FT])
        self.fit('C.NGLE.D',  [[OPCODE, 17], [FMT, 17], FT, FS, 5, [OPCODE, 57]],  [FS, FT])
        self.fit('C.SEQ.D',   [[OPCODE, 17], [FMT, 17], FT, FS, 5, [OPCODE, 58]],  [FS, FT])
        self.fit('C.NGL.D',   [[OPCODE, 17], [FMT, 17], FT, FS, 5, [OPCODE, 59]],  [FS, FT])
        self.fit('C.LT.D',    [[OPCODE, 17], [FMT, 17], FT, FS, 5, [OPCODE, 60]],  [FS, FT])
        self.fit('C.NGE.D',   [[OPCODE, 17], [FMT, 17], FT, FS, 5, [OPCODE, 61]],  [FS, FT])
        self.fit('C.LE.D',    [[OPCODE, 17], [FMT, 17], FT, FS, 5, [OPCODE, 62]],  [FS, FT])
        self.fit('C.NGT.D',   [[OPCODE, 17], [FMT, 17], FT, FS, 5, [OPCODE, 63]],  [FS, FT])

        # Floating-point Unit (COP1) Instructions continued...
        self.fit('ABS.S',      [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 5]],   [FD, FS])
        self.fit('ABS.D',      [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 5]],   [FD, FS])
        self.fit('ADD.S',      [[OPCODE, 17], [FMT, 16], FT, FS, FD, [OPCODE, 0]],  [FD, FS, FT])
        self.fit('ADD.D',      [[OPCODE, 17], [FMT, 17], FT, FS, FD, [OPCODE, 0]],  [FD, FS, FT])
        self.fit('BC1F',       [[OPCODE, 17], [FMT, 8], [FMT, 0], OFFSET],          [OFFSET])
        self.fit('BC1FL',      [[OPCODE, 17], [FMT, 8], [FMT, 2], OFFSET],          [OFFSET])
        self.fit('BC1T',       [[OPCODE, 17], [FMT, 8], [FMT, 1], OFFSET],          [OFFSET])
        self.fit('BC1TL',      [[OPCODE, 17], [FMT, 8], [FMT, 3], OFFSET],          [OFFSET])
        self.fit('CEIL.L.S',   [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 10]],  [FD, FS])
        self.fit('CEIL.L.D',   [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 10]],  [FD, FS])
        self.fit('CEIL.W.S',   [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 14]],  [FD, FS])
        self.fit('CEIL.W.D',   [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 14]],  [FD, FS])
        self.fit('CFC1',       [[OPCODE, 17], [FMT, 2], RT, FS, 11],                [RT, FS])
        self.fit('CTC1',       [[OPCODE, 17], [FMT, 6], RT, FS, 11],                [RT, FS])
        self.fit('CVT.D.S',    [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 33]],  [FD, FS])
        self.fit('CVT.D.W',    [[OPCODE, 17], [FMT, 20], 5, FS, FD, [OPCODE, 33]],  [FD, FS])
        self.fit('CVT.D.L',    [[OPCODE, 17], [FMT, 21], 5, FS, FD, [OPCODE, 33]],  [FD, FS])
        self.fit('CVT.L.S',    [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 37]],  [FD, FS])
        self.fit('CVT.L.D',    [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 37]],  [FD, FS])
        self.fit('CVT.S.D',    [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 32]],  [FD, FS])
        self.fit('CVT.S.W',    [[OPCODE, 17], [FMT, 20], 5, FS, FD, [OPCODE, 32]],  [FD, FS])
        self.fit('CVT.S.L',    [[OPCODE, 17], [FMT, 21], 5, FS, FD, [OPCODE, 32]],  [FD, FS])
        self.fit('CVT.W.S',    [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 36]],  [FD, FS])
        self.fit('CVT.W.D',    [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 36]],  [FD, FS])
        self.fit('DIV.S',      [[OPCODE, 17], [FMT, 16], FT, FS, FD, [OPCODE, 3]],  [FD, FS, FT])
        self.fit('DIV.D',      [[OPCODE, 17], [FMT, 17], FT, FS, FD, [OPCODE, 3]],  [FD, FS, FT])
        self.fit('DMFC1',      [[OPCODE, 17], [FMT, 1], RT, FS, 11],                [RT, FS])
        self.fit('DMTC1',      [[OPCODE, 17], [FMT, 5], RT, FS, 11],                [RT, FS])
        self.fit('FLOOR.L.S',  [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 11]],  [FD, FS])
        self.fit('FLOOR.L.D',  [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 11]],  [FD, FS])
        self.fit('FLOOR.W.S',  [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 15]],  [FD, FS])
        self.fit('FLOOR.W.D',  [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 15]],  [FD, FS])
        self.fit('LDC1',       [[OPCODE, 53], BASE, FT, IMMEDIATE],                 [FT, IMMEDIATE, BASE])
        self.fit('LWC1',       [[OPCODE, 49], BASE, FT, IMMEDIATE],                 [FT, IMMEDIATE, BASE])
        self.fit('MFC1',       [[OPCODE, 17], [FMT, 0], RT, FS, 11],                [RT, FS])
        self.fit('MOV.S',      [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 6]],   [FD, FS])
        self.fit('MOV.D',      [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 6]],   [FD, FS])
        self.fit('MTC1',       [[OPCODE, 17], [FMT, 4], RT, FS, 11],                [RT, FS])
        self.fit('MUL.S',      [[OPCODE, 17], [FMT, 16], FT, FS, FD, [OPCODE, 2]],  [FD, FS, FT])
        self.fit('MUL.D',      [[OPCODE, 17], [FMT, 17], FT, FS, FD, [OPCODE, 2]],  [FD, FS, FT])
        self.fit('NEG.S',      [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 7]],   [FD, FS])
        self.fit('NEG.D',      [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 7]],   [FD, FS])
        self.fit('ROUND.L.S',  [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 8]],   [FD, FS])
        self.fit('ROUND.L.D',  [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 8]],   [FD, FS])
        self.fit('ROUND.W.S',  [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 12]],  [FD, FS])
        self.fit('ROUND.W.D',  [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 12]],  [FD, FS])
        self.fit('SDC1',       [[OPCODE, 61], BASE, FT, IMMEDIATE],                 [FT, IMMEDIATE, BASE])
        self.fit('SQRT.S',     [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 4]],   [FD, FS])
        self.fit('SQRT.D',     [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 4]],   [FD, FS])
        self.fit('SUB.S',      [[OPCODE, 17], [FMT, 16], FT, FS, FD, [OPCODE, 1]],  [FD, FS, FT])
        self.fit('SUB.D',      [[OPCODE, 17], [FMT, 17], FT, FS, FD, [OPCODE, 1]],  [FD, FS, FT])
        self.fit('SWC1',       [[OPCODE, 57], BASE, FT, IMMEDIATE],                 [FT, IMMEDIATE, BASE])
        self.fit('TRUNC.L.S',  [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 9]],   [FD, FS])
        self.fit('TRUNC.L.D',  [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 9]],   [FD, FS])
        self.fit('TRUNC.W.S',  [[OPCODE, 17], [FMT, 16], 5, FS, FD, [OPCODE, 13]],  [FD, FS])
        self.fit('TRUNC.W.D',  [[OPCODE, 17], [FMT, 17], 5, FS, FD, [OPCODE, 13]],  [FD, FS])

        # Vector Load/Store
        # Opcode 50 and 51 may or may not need swapping
        self.fit('LBV', [[OPCODE, 50], BASE, VD, [EX_OPCODE, 0], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('LSV', [[OPCODE, 50], BASE, VD, [EX_OPCODE, 1], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('LLV', [[OPCODE, 50], BASE, VD, [EX_OPCODE, 2], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('LDV', [[OPCODE, 50], BASE, VD, [EX_OPCODE, 3], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('LQV', [[OPCODE, 50], BASE, VD, [EX_OPCODE, 4], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('LRV', [[OPCODE, 50], BASE, VD, [EX_OPCODE, 5], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('LPV', [[OPCODE, 50], BASE, VD, [EX_OPCODE, 6], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('LUV', [[OPCODE, 50], BASE, VD, [EX_OPCODE, 7], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('LHV', [[OPCODE, 50], BASE, VD, [EX_OPCODE, 8], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('LFV', [[OPCODE, 50], BASE, VD, [EX_OPCODE, 9], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('LWV', [[OPCODE, 50], BASE, VD, [EX_OPCODE, 10], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('LTV', [[OPCODE, 50], BASE, VD, [EX_OPCODE, 11], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('SBV', [[OPCODE, 51], BASE, VD, [EX_OPCODE, 0], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('SSV', [[OPCODE, 51], BASE, VD, [EX_OPCODE, 1], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('SLV', [[OPCODE, 51], BASE, VD, [EX_OPCODE, 2], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('SDV', [[OPCODE, 51], BASE, VD, [EX_OPCODE, 3], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('SQV', [[OPCODE, 51], BASE, VD, [EX_OPCODE, 4], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('SRV', [[OPCODE, 51], BASE, VD, [EX_OPCODE, 5], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('SPV', [[OPCODE, 51], BASE, VD, [EX_OPCODE, 6], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('SUV', [[OPCODE, 51], BASE, VD, [EX_OPCODE, 7], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('SHV', [[OPCODE, 51], BASE, VD, [EX_OPCODE, 8], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('SFV', [[OPCODE, 51], BASE, VD, [EX_OPCODE, 9], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('SWV', [[OPCODE, 51], BASE, VD, [EX_OPCODE, 10], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])
        self.fit('STV', [[OPCODE, 51], BASE, VD, [EX_OPCODE, 11], MOD, 1, VOFFSET], [VD, MOD, VOFFSET, BASE])

        # Vector Math
        self.fit('VMULF', [[OPCODE, 18], [CO, 1], MOD, VS, VT, VD, [OPCODE, 0]], [VD, VT, VS, MOD])
        self.fit('VMULU', [[OPCODE, 18], [CO, 1], MOD, VS, VT, VD, [OPCODE, 1]], [VD, VT, VS, MOD])
        self.fit('VRNDP', [[OPCODE, 18], [CO, 1], MOD, VS, VT, VD, [OPCODE, 2]], [VD, VT, VS, MOD])
        self.fit('VMULQ', [[OPCODE, 18], [CO, 1], MOD, VS, VT, VD, [OPCODE, 3]], [VD, VT, VS, MOD])
        self.fit('VMUDL', [[OPCODE, 18], [CO, 1], MOD, VS, VT, VD, [OPCODE, 4]], [VD, VT, VS, MOD])
        self.fit('VMUDM', [[OPCODE, 18], [CO, 1], MOD, VS, VT, VD, [OPCODE, 5]], [VD, VT, VS, MOD])
        self.fit('VMUDN', [[OPCODE, 18], [CO, 1], MOD, VS, VT, VD, [OPCODE, 6]], [VD, VT, VS, MOD])
        self.fit('VMUDH', [[OPCODE, 18], [CO, 1], MOD, VS, VT, VD, [OPCODE, 7]], [VD, VT, VS, MOD])
        self.fit('VMACF', [[OPCODE, 18], [CO, 1], MOD, VS, VT, VD, [OPCODE, 8]], [VD, VT, VS, MOD])
        self.fit('VMACU', [[OPCODE, 18], [CO, 1], MOD, VS, VT, VD, [OPCODE, 9]], [VD, VT, VS, MOD])
        self.fit('VRNDN', [[OPCODE, 18], [CO, 1], MOD, VS, VT, VD, [OPCODE, 10]], [VD, VT, VS, MOD])
        self.fit('VMACQ', [[OPCODE, 18], [CO, 1], MOD, VS, VT, VD, [OPCODE, 11]], [VD, VT, VS, MOD])
        self.fit('VMADL', [[OPCODE, 18], [CO, 1], MOD, VS, VT, VD, [OPCODE, 12]], [VD, VT, VS, MOD])
        self.fit('VMADM', [[OPCODE, 18], [CO, 1], MOD, VS, VT, VD, [OPCODE, 13]], [VD, VT, VS, MOD])
        self.fit('VMADN', [[OPCODE, 18], [CO, 1], MOD, VS, VT, VD, [OPCODE, 14]], [VD, VT, VS, MOD])
        self.fit('VMADH', [[OPCODE, 18], [CO, 1], MOD, VS, VT, VD, [OPCODE, 15]], [VD, VT, VS, MOD])
        # Unfinished

    def fit(self, mnemonic, encoding, appearance):
        appearance_bit_correspondence = {}
        matrix_branch = self.opcode_matrix
        bits_addressed = 0
        identifying_bits = '0b'
        new_encoding_segments = [[], [], []]
        # Makes sure EX_OPCODE is placed after all other named bit segments while processing and removing parameters
        for i in encoding:
            if isinstance(i, str):
                length = LENGTHS[i]
                identifying_bits += '0' * length
                shift_amount = 32 - (length + bits_addressed)
                appearance_bit_correspondence[i] = [
                    int('0b' + ('0' * bits_addressed) + ('1' * length) + ('0' * shift_amount), 2),
                    shift_amount
                ]
            elif isinstance(i, int):
                length = i
                identifying_bits += '0' * i
                new_encoding_segments[2].append((0, length, bits_addressed))
            else:
                value = i[1]
                name = i[0]
                length = LENGTHS[name]
                identifying_bits += extend_zeroes(bin(i[1])[2:], length)
                if name == EX_OPCODE:
                    new_encoding_segments[1].append((value, length, bits_addressed))
                else:
                    new_encoding_segments[0].append((value, length, bits_addressed))
            bits_addressed += length

        reordered_encoding = new_encoding_segments[0] + new_encoding_segments[1] + new_encoding_segments[2]

        # Fit the opcode matrix
        for j, i in enumerate(reordered_encoding):
            value, length, bits_addressed = i
            shift_amount = 32 - (length + bits_addressed)
            bitwise_and = int('0b' + ('1' * length) + ('0' * shift_amount), 2)
            if not matrix_branch:
                matrix_branch.append([])
                target = 0
            else:
                target = -1
                for i in range(len(matrix_branch)):
                    if matrix_branch[i][1] == bitwise_and and matrix_branch[i][2] == shift_amount:
                        target = i
                if target < 0:
                    target = len(matrix_branch)
                    matrix_branch.append([])
            if not matrix_branch[target]:
                matrix_branch[target].append([None] * (2 ** length))
                matrix_branch[target].append(bitwise_and)
                matrix_branch[target].append(shift_amount)
            if j < len(reordered_encoding) - 1:
                if matrix_branch[target][0][value] is None:
                    matrix_branch[target][0][value] = []
                matrix_branch = matrix_branch[target][0][value]
            else:
                matrix_branch[target][0][value] = mnemonic

        appearance = [[i, appearance_bit_correspondence[i]] for i in appearance]
        if mnemonic in DOCUMENTATION:
            self.documentation[mnemonic] = DOCUMENTATION[mnemonic]
        self.encodes[mnemonic] = encoding
        self.appearances[mnemonic] = appearance
        self.identifying_bits[mnemonic] = int(identifying_bits, 2)

    def decode(self, int_word, index):
        if int_word == 0:
            return 'NOP'
        mnemonic = None
        target_branch = self.opcode_matrix
        iterating = True
        while iterating:
            length = len(target_branch)
            for sub_branch in range(length):
                value = target_branch[sub_branch][0][(int_word & target_branch[sub_branch][1]) >> target_branch[sub_branch][2]]
                if value is None:
                    if sub_branch == length - 1:
                        iterating = False
                        break
                    continue
                elif isinstance(value, str):
                    mnemonic = value
                    iterating = False
                    break
                target_branch = value
                break

        if mnemonic is None:
            return ''
        parameters = ''
        index += 1  # Jump/branch to offsets/addresses are calculated based on the address of the delay slot (following word)
        for i in self.appearances[mnemonic]:
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
                        return ''
                    inner_value <<= 2
                if is_address:
                    # Add the current 268mb alignment to the value to properly decode
                    inner_value += index & 0x3C000000
                    inner_value <<= 2
                if self.game_address_mode and (is_address or is_offset):
                    inner_value += self.game_offset
                decode_text = self.immediate_identifier + extend_zeroes(hexi(inner_value), HEX_EXTEND[param_name])
            elif param_type == 'VHEX':
                decode_text = '[' + extend_zeroes(hexi(inner_value), HEX_EXTEND[param_name]) + ']'
            else: # Will be a register
                decode_text = REGISTERS[param_type][inner_value]
                if param_name == 'BASE':
                    decode_text = '(' + decode_text + ')'
            if param_type == 'VHEX':
                ''
            elif parameters != '':
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
        if punc < 0:
            punc = len(string)
        opcode = string[:punc]
        string = string[punc + 1:]
        if opcode not in self.encodes:
            # Syntax error - no such mnemonic
            return -1
        str_parameters = []
        index += 1  # Jump/branch to offsets/addresses are calculated based on the address of the delay slot (following word)
        try:
            while len(string):
                punc = string.find(', ')
                punc_2 = string.find('], ')
                cut = 2
                if punc < 0 or punc_2 >= 0:
                    punc = string.find('[')
                    cut = 1
                if punc < 0:
                    punc = string.find('], ')
                    cut = 3
                if punc < 0:
                    punc = string.find(' (')
                    cut = 2
                if punc < 0:
                    punc = string.find(')')
                    cut = 1
                if punc < 0:
                    punc = string.find(']')
                    cut = 1
                if punc < 0:
                    punc = len(string)
                    cut = 0
                str_parameters.append(string[:punc])
                string = string[punc + cut:]
                if len(str_parameters) == len(self.appearances[opcode]):
                    break
            int_result = self.identifying_bits[opcode]
            for i in range(len(self.appearances[opcode])):
                param = str_parameters[i]
                if param[0] == self.immediate_identifier or self.appearances[opcode][i][0] == 'MOD':
                    param = deci(param[1:])
                    is_address = self.appearances[opcode][i][0] == 'ADDRESS'
                    is_offset = self.appearances[opcode][i][0] == 'OFFSET'
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
                        if self.appearances[opcode][i][0] not in MAXIMUM_VALUES.keys():
                            # Syntax error - used immediate where a parameter should be a register
                            return -2
                        if param >> 2 > MAXIMUM_VALUES[self.appearances[opcode][i][0]] or param < 0:
                            # Immediate out of bounds error
                            return -3
                else:
                    param = REGISTERS_ENCODE[param]
                int_result += param << self.appearances[opcode][i][1][1]
        except:
            # Syntax error - wrong amount of parameters
            return -2
        return int_result

    def split_and_store_bytes(self, int_word, index):
        ints = get_8_bit_ints_from_32_bit_int(int_word)
        index <<= 2
        for i in range(4):
            self.hack_file[index + i] = ints[i]

    def map_jumps(self, text_box):
        try:
            if exists(self.jumps_file):
                with open(self.jumps_file, 'rb') as jumps_file:
                    self.jumps_to, self.branches_to = load(jumps_file)
                return
        except:
            # If we can't load the file, map the jumps again. No biggie.
            ''

        def dict_append(dict, key, value):
            try:
                dict[key].append(value)
            except KeyError:
                dict[key] = []
                dict[key].append(value)

        percent = (len(self.hack_file) >> 2) // 100
        cut = 19
        collect = 9
        buffer = [None] * (cut - 1)
        for i in range(0x40, len(self.hack_file), 4):
            int_word = int_of_4_byte_aligned_region(self.hack_file[i:i+4])
            j = i >> 2
            decoded = self.decode(int_word, j)
            if not decoded:
                del buffer
                buffer = []
                continue
            opcode = (int_word & 0xFC000000) >> 26
            if JUMP_INTS[opcode]:
                address = (int_word & 0x03FFFFFF) + ((j + 1) & 0x3C000000)
                if address > 16:
                    buffer.insert(0, (self.jumps_to, str(address), j))
            elif BRANCH_INTS[opcode] or decoded[:2] == 'BC':
                address = sign_16_bit_value(int_word & 0xFFFF) + j + 1
                buffer.insert(0, (self.branches_to, str(address), j))
            else:
                buffer.insert(0, None)
            if len(buffer) == cut:
                popped = buffer.pop(collect)
                if isinstance(popped, tuple):
                    dict_append(dict=popped[0],
                                key=popped[1],
                                value=popped[2])
            if not j & percent:
                text_box.delete('1.0', tkinter.END)
                text_box.insert('1.0', str(j // percent) + '%')
                text_box.update()
        with open(self.jumps_file, 'wb') as jumps_file:
            dump((self.jumps_to, self.branches_to), jumps_file)

    def find_jumps(self, index):
        this_function = []
        i = 0
        # Locate the top of the function
        # We can't really start mapping here, because it needs to find the bottom of the previous function first
        while i + index >= 0:
            navi = (index + i) << 2
            if navi < 0:
                return []
            int_word = int_of_4_byte_aligned_region(self.hack_file[navi:navi + 4])
            instruction = self.decode(int_word, index + i)
            if instruction[:5] == 'JR RA':
                # Skip the delay slot
                i += 2
                break
            i -= 1
        start_of_function = i + index
        # Map to the bottom of the function
        while i + index < len(self.hack_file) >> 2:
            navi = (index + i) << 2
            int_word = int_of_4_byte_aligned_region(self.hack_file[navi:navi + 4])
            instruction = self.decode(int_word, index + i)
            this_function.append(index + i)
            i += 1
            if instruction[:5] == 'JR RA':
                break
        end_of_function = i + index
        jumps = []
        for i in this_function:
            key = str(i)
            if key in self.jumps_to:
                offsetting = 0 if not self.game_address_mode else self.game_offset
                [jumps.append(extend_zeroes(hexi((j << 2) + offsetting), 8))
                 for j in self.jumps_to[key]]
        if self.game_address_mode:
            start_of_function += self.game_offset >> 2
            end_of_function += self.game_offset >> 2
        return jumps, start_of_function, end_of_function

    def unmap(self, dict, address, target):
        unmapped_target = False
        unmapped_address = False
        if key_in_dict(dict, target):
            try:
                place = dict[target].index(address)
                dict[target].pop(place)
                unmapped_address = True
                if not dict[target]:
                    del dict[target]
                    unmapped_target = True
            except:
                ''
        return unmapped_target, unmapped_address

    def map(self, dict, address, target):
        mapped_target = False
        mapped_address = False
        if not key_in_dict(dict, target):
            dict[target] = []
            mapped_target = True
        if address not in dict[target]:
            dict[target].append(address)
            mapped_address = True
        return mapped_target, mapped_address

    def calc_checksum(self):
        check_part = ints_of_4_byte_aligned_region(self.hack_file[0x1000:0x101000])
        t1 = t2 = t3 = t4 = t5 = t6 = 0xF8CA4DDC
        unsigned_long = lambda j: j & 0xFFFFFFFF
        ROL = lambda j, b: unsigned_long(j << b) | (j >> (32 - b))
        for c1 in check_part:
            k1 = unsigned_long(t6 + c1)
            if k1 < t6:
                t4 = unsigned_long(t4 + 1)
            t6 = k1
            t3 ^= c1
            k2 = c1 & 0x1F
            k1 = ROL(c1, k2)
            t5 = unsigned_long(t5 + k1)
            if c1 < t2:
                t2 ^= k1
            else:
                t2 ^= t6 ^ c1
            t1 = unsigned_long(t1 + (c1 ^ t5))
        sum1 = t6 ^ t4 ^ t3
        sum2 = t5 ^ t2 ^ t1
        self.split_and_store_bytes(sum1, self.header_items['CRC1'][0] >> 2)
        self.split_and_store_bytes(sum2, self.header_items['CRC2'][0] >> 2)
        return sum1, sum2

    def find_vector_instructions(self):
        ints = ints_of_4_byte_aligned_region(self.hack_file[0x1000:])
        missing_opcodes = [True if i in [18, 19, 28, 29, 30, 31, 50, 51, 54, 58, 59, 62] else False for i in range(64)]
        # missing_opcodes = [True if i in [18] else False for i in range(64)]
        # return_dict = {
        #     '18': [],
        #     '19': [],
        #     '28': [],
        #     '29': [],
        #     '30': [],
        #     '31': [],
        #     '50': [],
        #     '51': [],
        #     '54': [],
        #     '58': [],
        #     '59': [],
        #     '62': []
        # }
        # in testing stage
        for i, instruction in enumerate(ints):
            decoded = self.decode(instruction, i + 0x400)
            if decoded == 'UNKNOWN/NOT AN INSTRUCTION':
                opcode = (instruction & 0xFC000000) >> 26
                if missing_opcodes[opcode]:
                    self.comments[str(i + 0x400)] = extend_zeroes(bin(instruction)[2:], 32)
                    # return_dict[str(opcode)].append((extend_zeroes(hexi(i << 2),8), instruction))
        # return return_dict


































