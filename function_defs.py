
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


def deci(hex_num):
    if not hex_num:
        return 0
    return int('0x' + hex_num if '0x' not in hex_num else hex_num,16)


def hexi(dec_num):
    return hex(dec_num)[2:].upper()


def extend_zeroes(str,amount):
    return '0' * (amount - len(str)) + str


def hex_of_4_byte_aligned_region(bytes):
    return [bytes[i:i+4].hex() for i in range(0, len(bytes), 4)]


def ints_of_4_byte_aligned_region(bytes, byteorder = 'big'):
    return [int.from_bytes(bytes[i:i+4], byteorder = byteorder, signed = False) for i in range(0, len(bytes), 4)]


def int_of_4_byte_aligned_region(bytes, byteorder = 'big'):
    return int.from_bytes(bytes, byteorder = byteorder, signed = False)


def sign_16_bit_value(int):
    return int - 65536 if int & 32768 else int


def unsign_16_bit_value(int):
    return int + 65536 if int < 0 else int


def keep_within(int, min, max):
    return max if int > max else (min if int < min else int)


def get_8_bit_ints_from_32_bit_int(int):
    return (int & 0xFF000000) >> 24, (int & 0xFF0000) >> 16, (int & 0xFF00) >> 8, int & 0xFF


# Align the value to the nearest step
def align_value(value, step):
    return value - (value % step)


# To translate the comments dict into a negotiable text document formatted string
def dict_to_string(dict):
    return '\n'.join(['{}: {}'.format(extend_zeroes(hexi(i << 2), 8), dict['{}'.format(i)])
                      for i in sorted([int(key) for key in dict])])


def split_at_points(string, *points):
    splitting = []
    prev = 0
    for i in points:
        splitting.append(string[prev:i])
        prev = i
    splitting.append(string[prev:])
    return ' '.join(splitting)


# And to translate the comments from the file back into the comments dict
def string_to_dict(str):
    str_list = str.split('\n')
    result_dict = {}
    for i in range(len(str_list)):
        try:
            result_dict['{}'.format(int('0x' + str_list[i][:8], 16) >> 2)] = str_list[i][10:]
        except:
            limit = len(str_list) - 1
            block = [('   >>>>>\t' if j == i else '\t') +
                     str_list[j] for j in range(keep_within(i - 2, 0, limit),
                                                keep_within(i + 3, 0, limit))]
            raise Exception('Error loading at line {}:\n{}'.format(i + 1, '\n'.join(block)))
    return result_dict


last_time = 0


def timer_reset():
    global last_time
    last_time = time.time()


def timer_tick(string):
    global last_time
    this_time = time.time() - last_time
    last_time = time.time()
    str_time = str(this_time)
    print('{} took: {} sec'.format(string, str_time[:str_time.find('.') + 4]))


def timer_get():
    global last_time
    this_time = time.time() - last_time
    last_time = time.time()
    str_time = str(this_time)
    return float(str_time[:str_time.find('.') + 4])


def nice_time(_t=None):
    if not _t:
        _t = time.time()
    days = ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun']
    t = time.localtime(_t)
    return '{}:{}:{} - {} {}/{}/{}'.format(t.tm_hour, t.tm_min, t.tm_sec, days[t.tm_wday], t.tm_mday, t.tm_mon, t.tm_year)
