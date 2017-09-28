
from os.path import exists
from pickle import dump, load
import time


def pickle_data(object, name):
    if not name:
        return
    with open(name, 'wb') as f:
        dump(object, f)


def unpickle_data(name):
    if not name or not exists(name):
        return
    with open(name, 'rb') as f:
        return load(f)


def type_of(var):
    return type(var).__name__


def deci(hex_num):
    return int('0x' + hex_num if '0x' not in hex_num else hex_num,16)


def hexi(dec_num):
    return hex(dec_num)[2:].upper()


def hex_space(string):
    return ' '.join([string[i:i+2] for i in range(0, len(string), 2)])


def extend_zeroes(str,amount):
    return '0' * (amount - len(str)) + str


def hex_of_4_byte_aligned_region(bytes):
    return [bytes[i:i+4].hex() for i in range(0, len(bytes), 4)]


def ints_of_4_byte_aligned_region(bytes, byteorder = 'big'):
    return [int.from_bytes(bytes[i:i+4], byteorder = byteorder, signed = False) for i in range(0, len(bytes), 4)]


def sign_16_bit_value(int):
    return int - 65536 if int & 32768 else int


def unsign_16_bit_value(int):
    return int + 65536 if int < 0 else int


def keep_within(int, min, max):
    return max if int > max else (min if int < min else int)


def get_8_bit_ints_from_32_bit_int(int):
    return (int & 4278190080) >> 24, (int & 16711680) >> 16, (int & 65280) >> 8, int & 255


def align_value(value, alignment):
    return value - (value % alignment)


# To translate the comments dict into a negotiable text document formatted string
def dict_to_string(dict):
    return '\n'.join(['{}: {}'.format(extend_zeroes(hexi(i << 2), 8), dict['{}'.format(i)])
                      for i in sorted([int(key) for key in dict])])


# And to translate the comments from the file back into the comments dict
def string_to_dict(str):
    str_list = str.split('\n')
    result_dict = {}
    for i in str_list:
        result_dict['{}'.format(int('0x' + i[:8], 16) >> 2)] = i[10:]
    return result_dict


last_time = 0


def timer_reset():
    global last_time
    last_time = time.time()


def timer_tick(string):
    global last_time
    this_time = time.time() - last_time
    last_time = time.time()
    print('{} took: {} sec'.format(string, this_time))

