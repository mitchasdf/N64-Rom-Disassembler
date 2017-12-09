
import tkinter as tk
import tkinter.font as tkfont
from tkinter import simpledialog, filedialog, colorchooser
import os
from function_defs import *
from disassembler import Disassembler, REGISTERS_ENCODE, BRANCH_INTS, JUMP_INTS, CIC, DOCUMENTATION
import webbrowser


CONFIG_FILE = 'rom disassembler.config'

BRANCH_FUNCTIONS = ['BEQ', 'BEQL', 'BGEZ', 'BGEZAL', 'BGEZALL', 'BGEZL', 'BGTZ', 'BGTZL', 'BLEZ', 'BLEZL',
                    'BLTZ', 'BLTZAL', 'BLTZALL', 'BLTZL', 'BNEZ', 'BNEL', 'BNE', 'BC1F', 'BC1FL', 'BC1T', 'BC1TL']

JUMP_FUNCTIONS = ['J', 'JR', 'JAL', 'JALR']

# Disassembler, created when opening files
disasm = None

window = tk.Tk()

working_dir = os.path.dirname(os.path.realpath(__file__)) + '\\'
FRESH_APP_CONFIG = {
    'previous_base_location': working_dir,
    'previous_hack_location': working_dir,
    'previous_base_opened': '',
    'previous_hack_opened': '',
    'open_roms_automatically': False,
    'window_geometry': '1133x609',
    'hack_of_base': {},
    'font_size': 10,
    'max_lines': 40,
    'scroll_amount': 8,
    'toggle_base_file': True,
    'immediate_identifier': '$',
    'hex_mode': False,
    'hex_space_separation': True,
    'game_address_mode': {},
    'CIC': {},
    'calc_crc': {},
    'auto_copy': 0,
    'status_bar': True,
    'comments_auto_focus_comments': False,
    'comments_auto_focus_hack': False,
    'jumps_auto_focus_comments': False,
    'jumps_auto_focus_hack': False,
    'mem_edit_offset': {},
    'jumps_displaying': {},
    'cursor_line_colour': '#686868',
    'window_background_colour': '#606060',
    'text_bg_colour': '#454545',
    'text_fg_colour': '#D0D0D0',
    'tag_config': {
          'jump_to': '#1d6152',
          'branch_to': '#1f5d11',
          'jump_from': '#D06060',
          'target': '#009880',
          'branch': '#985845',
          'jump': '#A2463C',
          'bad': '#DD3333',
          'out_of_range': '#BB2222',
          'nop': '#606060',
          'function_end': '#303060',
          'liken': '#308A30'
    }
}

# Setup app_config either fresh or from file
if os.path.exists(CONFIG_FILE):
    try:
        # Allows me to easily modify the app config items
        config_in_file = unpickle_data(CONFIG_FILE)
        config_keys = [key for key in config_in_file]
        fresh_keys = [key for key in FRESH_APP_CONFIG]
        for key in fresh_keys:
            if key not in config_keys:
                config_in_file[key] = FRESH_APP_CONFIG[key]
        for key in config_keys:
            if key not in fresh_keys:
                del config_in_file[key]
        app_config = config_in_file.copy()
        del app_config['tag_config']
        app_config['tag_config'] = {}
        for tag in FRESH_APP_CONFIG['tag_config']:
            try:
                app_config['tag_config'][tag] = config_in_file['tag_config'][tag]
            except:
                app_config['tag_config'][tag] = FRESH_APP_CONFIG['tag_config'][tag]
    except Exception as e:
        app_config = FRESH_APP_CONFIG.copy()
        simpledialog.messagebox._show('Error',
                                      'There was a problem loading the config file. '
                                      'Starting with default configuration.'
                                      '\n\nError: {}'.format(e))
        # raise Exception(e)
else:
    app_config = FRESH_APP_CONFIG.copy()

app_config['window_geometry'] = FRESH_APP_CONFIG['window_geometry']
window.title('ROM Disassembler')
window.geometry('{}+5+5'.format(app_config['window_geometry']))
window.iconbitmap('n64_disassembler.ico')
window.config(bg=app_config['window_background_colour'])


'''
    A GUI with custom behaviour is required for user-friendliness.
    
    Displaying the whole file at once in a text box causes lag, so these text boxes will
      need to hold a small amount of data at a time in order to run smoothly.
      
    This can be done by maintain max_lines amount of lines at all times.
    
    Deviating from max_lines causes the list containing the data for the syntax checker
      to have a "line shift". The syntax checker can't assume where, or whether or not, a shift
      has happened, so it needs the data to be processed before the checker receives it.
    
    The only times the amount of lines will change is when:
      - User has pressed: Enter
                          BackSpace (if cursor.line != 1 and cursor.column == 0)
                          Delete or Ctrl+D (if cursor.line != max_lines and cursor.column == end_of_column)
      - 1 or more highlighted (or selected) newlines are replaced (or "typed/pasted over")
      - Data containing 1 or more newlines is pasted into the text box
      
    Conditional management of each keypress is required to stop all of those problems from happening.
'''


address_text_box = tk.Text(window)
base_file_text_box = tk.Text(window)
hack_file_text_box = tk.Text(window)
comments_text_box = tk.Text(window)

ALL_TEXT_BOXES = [address_text_box,
                  base_file_text_box,
                  hack_file_text_box,
                  comments_text_box]
[text_box.config(state=tk.DISABLED) for text_box in ALL_TEXT_BOXES]

# [current_buffer_position, [(navigation, cursor_location, text_box_content
# , immediate_id, game_address_mode, hex_mode), ...]]
# |-----------for hack_buffer only----------|
hack_buffer = [-1, []]
comments_buffer = [-1, []]
buffer_max = 20000

# {'decimal_address': [error_code, text_attempted_to_encode], ...}
user_errors = {}

max_lines = app_config['max_lines']
main_font_size = app_config['font_size']
navigation = 0


def font_dimension(size):
    fonty = tkfont.Font(family='Courier', size=size, weight='normal')
    return fonty.measure(' '), fonty.metrics('linespace')


def save_config():
    pickle_data(app_config, CONFIG_FILE)


def get_colours_of_hex(hex_code):
    int_colour = int('0x' + hex_code[1:], 16)
    r, g, b = (int_colour & 0xFF0000) >> 16, (int_colour & 0xFF00) >> 8, int_colour & 0xFF
    return r, g, b


def solve_against_greyscale(r, g, b):
    grey_scale = (r + g + b) / 3
    if grey_scale < 128:
        colour = '#FFFFFF'
    else:
        colour = '#000000'
    return colour


def change_colours():
    text_bg = app_config['text_bg_colour']
    text_fg = app_config['text_fg_colour']
    win_bg = app_config['window_background_colour']
    cursor_line_colour = app_config['cursor_line_colour']
    new_tag_config = app_config['tag_config']
    [hack_file_text_box.tag_delete(tag) for tag in app_config['tag_config']]
    [(text_box.tag_delete('cursor_line'),
      text_box.tag_config('cursor_line', background=cursor_line_colour))
     for text_box in ALL_TEXT_BOXES]
    [hack_file_text_box.tag_config(tag, background=new_tag_config[tag]) for tag in new_tag_config]
    [text_box.config(bg=text_bg, fg=text_fg) for text_box in ALL_TEXT_BOXES + [address_input, address_output, status_bar]]
    [button.config(bg=win_bg, fg=text_fg, activebackground=win_bg,activeforeground=text_fg) for button in
     [address_translate_button, nav_button]]
    auto_copy_checkbtn.config(bg=win_bg, fg=text_fg, activebackground=win_bg,activeforeground=text_fg, selectcolor=win_bg)
    window.config(bg=win_bg)
    [label.config(bg=new_tag_config['target']) for label in [target_down_label, target_up_label]]
    [label.config(bg=new_tag_config['jump_from']) for label in [target_of_down_label, target_of_up_label]]
    if change_rom_name_button:
        change_rom_name_button.config(bg=text_bg, fg=text_fg, activebackground=text_bg, activeforeground=text_fg)
    r, g, b = get_colours_of_hex(text_bg)
    new_insert_colour = solve_against_greyscale(r, g, b)
    [text_box.config(insertbackground=new_insert_colour) for text_box in [address_input, address_output] + ALL_TEXT_BOXES]
    highlight_stuff(skip_moving_cursor=True)


def disassembler_loaded():
    if disasm:
        if disasm.loaded:
            return True
    return False


def check_widget(widget):
    if not widget is hack_file_text_box and not widget is comments_text_box and \
       not widget is address_text_box and not widget is base_file_text_box:
        widget = None
    return widget


def get_text_content(handle):
    text_content = handle.get('1.0', tk.END)
    # In case other machines don't append an extra \n character for no reason like mine does
    if '\n'.count(text_content) == max_lines:
        text_content = text_content[:-1]
    return text_content


def hex_space(string):
    if not app_config['hex_space_separation']:
        return string
    return ' '.join([string[i:i+2] for i in range(0, len(string), 2)])


def clear_error(key):
    if isinstance(key, int):
        key = '{}'.format(key)
    if key_in_dict(user_errors, key):
        del user_errors[key]


def cursor_value(line, column):
    return '{}.{}'.format(line, column)


def get_cursor(handle, cursor_tag = tk.INSERT):
    cursor = handle.index(cursor_tag)
    dot = cursor.find('.')
    line = int(cursor[:dot])
    column = int(cursor[dot + 1:])
    return cursor, line, column


# To easily move cursor by x,y amount or floor/ceil the column
def modify_cursor(cursor, line_amount, column_amount, text):
    # cursor value format:
    # '{line}.{column}'
    #  1-base  0-base
    if isinstance(text, str):
        text = text.split('\n')
    dot = cursor.find('.')
    line = int(cursor[:dot])
    column = int(cursor[dot + 1:])
    line = keep_within(line + line_amount, 1, len(text))
    line_length = len(text[line - 1])
    if isinstance(column_amount, int):
        column = keep_within(column + column_amount, 0, line_length)
    else:
        if column_amount == 'min':
            column = 0
        if column_amount == 'max':
            column = line_length
    return cursor_value(line, column), line, column


def geometry(geo):
    # geometry format:
    # '{width}x{height}+{x_pos}+{y_pos}'
    #                  |---optional---|
    #                  |-when setting-|
    mul_symbol = geo.find('x')
    plus_symbol_one = geo.find('+')
    plus_symbol_two = geo.find('+', plus_symbol_one + 1)
    window_w = int(geo[:mul_symbol])
    window_h = int(geo[mul_symbol + 1:plus_symbol_one])
    window_x = int(geo[plus_symbol_one + 1:plus_symbol_two])
    window_y = int(geo[plus_symbol_two + 1:])
    return window_w, window_h, window_x, window_y


def get_word_at(list, line, column):
    line_text = list[line - 1]
    lower_bound_punc = line_text.rfind('(', 0, column)
    # if lower_bound_punc < 0:
    #     lower_bound_punc = line_text.rfind('[', 0, column)
    if lower_bound_punc < 0:
        lower_bound_punc = line_text.rfind(' ', 0, column)
    upper_bound_punc = line_text.find(',', column)
    upp_punc_2 = line_text.find('], ', column)
    if upper_bound_punc < 0 or upp_punc_2 >= 0:
        upper_bound_punc = line_text.find('[', column)
    if upper_bound_punc < 0:
        upper_bound_punc = line_text.find('], ', column)
    if upper_bound_punc < 0:
        upper_bound_punc = line_text.find(')', column)
    if upper_bound_punc < 0:
        upper_bound_punc = len(line_text)
    return list[line - 1][lower_bound_punc + 1: upper_bound_punc]


# Without this, if the mouse_x location is anywhere beyond halfway between the x positions of
#   the end character and the right end of the text box, the text input cursor will move to
#   the beginning of the next line
def correct_cursor(event):
    try:
        handle, cursor_x, cursor_y = event.widget, event.x, event.y
        x, y, w, h = handle.bbox(tk.INSERT)
        if cursor_x != keep_within(cursor_x, x, x+w) or cursor_y != keep_within(cursor_y, y, y+h):
            cursor, line, column = get_cursor(handle)
            if not column:
                text_content = get_text_content(handle)
                handle.mark_set(tk.INSERT, modify_cursor(cursor, -1, 'max', text_content)[0])
    except:
        ''


# Is called pretty much after every time the view is changed
prev_reg_target, prev_address_target, prev_cursor_location = '', 0, 0
def highlight_stuff(widget=None, skip_moving_cursor=False):
    global prev_reg_target, prev_address_target, prev_cursor_location, targ_direction
    if not disassembler_loaded():
        return
    [hack_file_text_box.tag_remove(tag, '1.0', tk.END) for tag in app_config['tag_config']]
    [text_box.tag_remove('cursor_line', '1.0', tk.END) for text_box in ALL_TEXT_BOXES]

    if not widget:
        cursor, c_line, column = get_cursor(hack_file_text_box)
        hack_function = True
    else:
        cursor, c_line, column = get_cursor(widget)
        hack_function = True if widget is hack_file_text_box else False
    text = get_text_content(hack_file_text_box).split('\n')
    targeting = get_word_at(text, c_line, column)

    if not prev_cursor_location:
        prev_cursor_location = navigation + c_line - 1
    elif not skip_moving_cursor:
        this_handle = hack_file_text_box if not widget else widget
        new_cursor = None
        if prev_cursor_location <= navigation:
            new_cursor = '1.0'
        elif prev_cursor_location > navigation + max_lines:
            new_cursor = cursor_value(max_lines, 0)
        elif prev_cursor_location in range(navigation, navigation + max_lines):
            new_cursor = cursor_value(prev_cursor_location - navigation, 0)
            new_cursor = modify_cursor(new_cursor, 1, 'max', get_text_content(this_handle))[0]
        if new_cursor:
            this_handle.mark_set(tk.INSERT, new_cursor)
            cursor, c_line, column = get_cursor(this_handle)

    new_text = ''
    if (widget is hack_file_text_box or widget is base_file_text_box) and prev_cursor_location >= 0x10:
        file_navi = prev_cursor_location << 2
        file = disasm.hack_file if widget is hack_file_text_box else disasm.base_file
        decoded = disasm.decode(int_of_4_byte_aligned_region(file[file_navi:file_navi+4]), prev_cursor_location)
        cut = decoded.find(' ')
        if cut < 0:
            cut = len(decoded)
        mnemonic = decoded[:cut]
        if mnemonic in disasm.documentation:
            new_text = '{}: {}'.format(mnemonic, disasm.documentation[mnemonic])
    if new_text:
        status_text.set(new_text)

    jumps_from = {}

    if prev_cursor_location in range(navigation, navigation + max_lines):
        [text_box.tag_add('cursor_line',
                          cursor_value(c_line, 0),
                          cursor_value(c_line + 1, 0))
         for text_box in ALL_TEXT_BOXES]

    if prev_address_target or not column:
        c_line = 0

    address = None if c_line else prev_address_target
    for i in range(len(text)):
        navi = navigation + i
        if not app_config['hex_mode']:
            line = i + 1
            line_text = text[i]
            this_word = line_text[:line_text.find(' ')]
            imm_id = text[i].find(app_config['immediate_identifier'])

            # Highlight the end of each function
            if text[i] == 'JR RA':
                hack_file_text_box.tag_add('function_end',
                                           cursor_value(line, 0),
                                           cursor_value(line, 5))

            # Highlight branch functions
            elif this_word in BRANCH_FUNCTIONS:
                hack_file_text_box.tag_add('branch',
                                           cursor_value(line, 0),
                                           cursor_value(line, len(text[i])))
                hex_address = line_text[imm_id + 1:]
                if line == c_line:
                    address = hex_address
                try:
                    possibly_value_error = (deci(hex_address) - (disasm.game_offset if disasm.game_address_mode else 0)) >> 2
                    jumps_from[str(navi)] = possibly_value_error
                except ValueError:
                    ''

            # Highlight jump functions
            elif this_word in JUMP_FUNCTIONS:
                hack_file_text_box.tag_add('jump',
                                           cursor_value(line, 0),
                                           cursor_value(line, len(text[i])))
                hex_address = line_text[imm_id + 1:]
                if line == c_line and this_word in ['J', 'JAL']:
                    address = hex_address
                try:
                    possibly_value_error = (deci(hex_address) - (disasm.game_offset if disasm.game_address_mode else 0)) >> 2
                    jumps_from[str(navi)] = possibly_value_error
                except ValueError as e:
                    ''

            elif line_text == 'NOP':
                hack_file_text_box.tag_add('nop',
                                           cursor_value(line, 0),
                                           cursor_value(line, len(text[i])))

            # Highlight instructions in which are a target of any jump or branch
            # Because the disasm.jumps dict is so huge, a try/except works "exceptionally" faster than iterative methods
            try:
                _ = disasm.jumps_to[str(navi)]
                hack_file_text_box.tag_add('jump_to',
                                           cursor_value(line, 0),
                                           cursor_value(line + 1, 0))
            except KeyError:
                ''

            try:
                _ = disasm.branches_to[str(navi)]
                hack_file_text_box.tag_add('branch_to',
                                           cursor_value(line, 0),
                                           cursor_value(line + 1, 0))
            except KeyError:
                ''

        # Highlight errors
        key = str(navi)
        if key_in_dict(user_errors, key):
            err_code = user_errors[key][0]
            hack_file_text_box.tag_add('bad' if err_code > -3 else 'out_of_range',
                                       cursor_value(i + 1, 0),
                                       cursor_value(i + 2, 0))

    # Highlight jumps/branches to target instruction
    if hack_function:
        this_key = str(prev_cursor_location)
        dictie = None
        if key_in_dict(disasm.branches_to, this_key):
            dictie = disasm.branches_to
        elif key_in_dict(disasm.jumps_to, this_key):
            dictie = disasm.jumps_to
        if dictie:
            had_ups = False
            had_downs = False
            for i in dictie[this_key]:
                place = i - navigation
                if place in range(max_lines):
                    hack_file_text_box.tag_add('jump_from',
                                               cursor_value(place + 1, 0),
                                               cursor_value(place + 2, 0))
                elif place >= max_lines:
                    had_downs = True
                    target_of_down()
                else:
                    had_ups = True
                    target_of_up()
            if not had_ups:
                target_of_up_label.place_forget()
            if not had_downs:
                target_of_down_label.place_forget()
        else:
            target_of_down_label.place_forget()
            target_of_up_label.place_forget()

    if address:
        if hack_function:
            if c_line:
                try:
                    # Raises error if user types non-numeric characters where an address/offset is
                    address = deci(address)
                except:
                    address = -1
                else:
                    if disasm.game_address_mode:
                        address -= disasm.game_offset
                    address >>= 2
                    prev_address_target = address
                    jumps_from[str((c_line + navigation) - 1)] = address
            if address in range(navigation, navigation + max_lines):
                target_none()
                place = address - navigation
                hack_file_text_box.tag_add('target',
                                           cursor_value(place + 1, 0),
                                           cursor_value(place + 2, 0))
            elif address < navigation:
                target_up()
            elif address >= navigation + max_lines:
                target_down()

    # Highlight all of the same registers on screen if cursor is targeting one
    def highlight_targets(target):
        for i in range(len(text)):
            line = i + 1
            begin = 0
            while hack_function:
                column = text[i].find(target, begin)
                word_at = get_word_at(text, line, column)
                if column >= 0:
                    if word_at[:1] != app_config['immediate_identifier']:
                        hack_file_text_box.tag_add('liken',
                                                   cursor_value(i + 1, column),
                                                   cursor_value(i + 1, column + len(target)))
                else:
                    break
                begin = column + 1

    # These conditions allow user to scroll out of view of the target without losing highlighting
    if prev_reg_target:
        highlight_targets(prev_reg_target)
    elif targeting in REGISTERS_ENCODE:
        prev_reg_target = targeting
        highlight_targets(targeting)


def reset_target():
    global prev_reg_target, prev_address_target, prev_cursor_location
    prev_reg_target, prev_address_target = '', 0
    prev_cursor_location = 0
    target_none()


# The hacked text box syntax checker and change applier
def apply_hack_changes(ignore_slot = None):
    if not disassembler_loaded():
        return

    targets_lower = [deci(i[:8]) >> 2 for i in jumps_displaying]
    targets_upper = [deci(i[11:19]) >> 2 for i in jumps_displaying]
    def find_target_key(target):
        if disasm.game_address_mode:
            target += disasm.game_offset >> 2
        i = 0
        target_key = ''
        while i < len(jumps_displaying):
            if target in range(targets_lower[i], targets_upper[i]):
                target_key = list(jumps_displaying)[i]
                break
            i += 1
        return target_key

    def unmap(decoded_word, navi):
        target = 0
        dic = {}
        if decoded_word in ['J', 'JAL']:
            target = disasm.solve_address(navi + 1, int_word & 0x03FFFFFF) >> 2
            dic = disasm.jumps_to
        elif decoded_word in BRANCH_FUNCTIONS:
            target = sign_16_bit_value(int_word & 0xFFFF) + navi + 1
            dic = disasm.branches_to
        if dic:
            str_target = str(target)
            unmapped_target, unmapped_address = disasm.unmap(dic, navi, str_target)
            if dic is disasm.jumps_to:
                target_key = find_target_key(target)
                if unmapped_address and target_key:
                    address = extend_zeroes(hexi((navi << 2) + (disasm.game_offset if disasm.game_address_mode else 0)), 8)
                    cut_jumps = [i[:8] for i in jumps_displaying[target_key]]
                    if address in cut_jumps:
                        place = cut_jumps.index(address)
                        jumps_displaying[target_key].pop(place)
                        if not jumps_displaying[target_key]:
                            del jumps_displaying[target_key]
                            if jumps_window:
                                try:
                                    place = function_list_box.get(0, tk.END).index(target_key)
                                    function_list_box.delete(place)
                                except:
                                    ''
                        if jumps_window:
                            try:
                                place = jump_list_box.get(0, tk.END).index(address)
                                jump_list_box.delete(place)
                            except:
                                ''

    current_text = get_text_content(hack_file_text_box).upper()
    split_text = current_text.split('\n')
    lines = min([max_lines, len(split_text)])
    for i in range(lines):
        navi = navigation + i
        file_nav = navi << 2
        if i == ignore_slot:
            continue
        is_hex_part = navi < 16
        int_word = int_of_4_byte_aligned_region(disasm.hack_file[file_nav:file_nav+4])
        decoded = disasm.decode(int_word, navi)
        decode_place = decoded.find(' ')
        if decode_place >= 0:
            decoded_word = decoded[:decode_place]
        else:
            decoded_word = decoded
        this_place = split_text[i].find(' ')
        if this_place >= 0:
            this_word = split_text[i][:this_place]
        else:
            this_word = split_text[i]
        string_key = '{}'.format(navi)
        # key_4 = extend_zeroes(hexi(navi << 2), 8)
        if not decoded:
            decoded = 'widdley scuds boi'
        if decoded == split_text[i]:
            clear_error(string_key)
        elif is_hex_part or app_config['hex_mode']:
            without_spaces = split_text[i].replace(' ', '')
            try:
                if len(without_spaces) not in range(0, 9):
                    raise Exception()
                int_of = deci(without_spaces)
            except:
                user_errors[string_key] = (-1, split_text[i])
                continue

            disasm.split_and_store_bytes(int_of, navi)
            clear_error(string_key)
            if not is_hex_part:
                unmap(decoded_word, navi)

        elif not split_text[i]:
            disasm.split_and_store_bytes(0, navi)
            hack_file_text_box.insert(cursor_value(i + 1, 0), 'NOP')
            clear_error(string_key)
            unmap(decoded_word, navi)

        elif split_text[i] != 'UNKNOWN/NOT AN INSTRUCTION':
            encoded_int = disasm.encode(split_text[i], navi)
            if encoded_int >= 0:
                disasm.split_and_store_bytes(encoded_int, navi)
                clear_error(string_key)
                unmap(decoded_word, navi)
                if this_word in ['J', 'JAL'] + BRANCH_FUNCTIONS:
                    try:
                        found = split_text[i].rfind(app_config['immediate_identifier'])
                        if found < 0:
                            raise Exception()
                        target = split_text[i][found + 1:]
                        target = deci(target)
                        if disasm.game_address_mode:
                            target -= disasm.game_offset
                        str_target = str(target >> 2)
                        if this_word in ['J', 'JAL']:
                            dic = disasm.jumps_to
                        else:
                            dic = disasm.branches_to
                        mapped_target, mapped_address = disasm.map(dic, navi, str_target)
                        if mapped_address:
                            target_key = find_target_key(target >> 2)
                            if target_key:
                                address = extend_zeroes(hexi((navi << 2) + (disasm.game_offset if disasm.game_address_mode else 0)), 8)
                                if address not in jumps_displaying[target_key]:
                                    jumps_displaying[target_key].append(address)
                                    if jumps_window and function_select == target_key:
                                        jump_list_box.insert(tk.END, address)

                    except Exception as e:
                        ''
            else:
                user_errors[string_key] = (encoded_int, split_text[i])


# Disassembler.comments accumulator
def apply_comment_changes():
    if not disassembler_loaded():
        return
    current_text = get_text_content(comments_text_box)
    split_text = current_text.split('\n')
    config = jumps_displaying.copy()
    increment = disasm.game_offset if disasm.game_address_mode else 0
    if comments_window:
        filtering = filter_text.get('1.0', tk.END).split('\n')[0].lower()
        comments_in = comments_list.get(0, tk.END)
        addresses = [j[:8] for j in comments_in]
        addresses_dict = {}
        for j, i in enumerate(addresses):
            addresses_dict[i] = j
        int_addresses = [deci(j) for j in addresses]
    orig_keys = {}
    for key in config:
        orig_keys[key[:8]] = key
    lines = min([max_lines, len(split_text)])
    for i in range(lines):
        navi = navigation + i
        string_key = '{}'.format(navi)
        hex_navi = extend_zeroes(hexi((navi << 2) + increment), 8)
        if not split_text[i] and key_in_dict(disasm.comments, string_key):
            del disasm.comments[string_key]
            if comments_window:
                if key_in_dict(addresses_dict, hex_navi):
                    comments_list.delete(addresses_dict[hex_navi])
            if key_in_dict(orig_keys, hex_navi):
                del jumps_displaying[orig_keys[hex_navi]]
                new_key = orig_keys[hex_navi][:19]
                jumps_displaying[new_key] = config[orig_keys[hex_navi]]
                if jumps_window:
                    try:
                        index = function_list_box.get(0, tk.END).index(orig_keys[hex_navi])
                        function_list_box.delete(index)
                        function_list_box.insert(index, new_key)
                    except ValueError:
                        ''
            breaking = False
            for key in config:
                for l, address in enumerate(config[key]):
                    if address[:8] == hex_navi:
                        dictin = config[key]
                        dictin[l] = address[:8]
                        if jumps_window:
                            try:
                                index = jump_list_box.get(0, tk.END).index(address)
                                jump_list_box.delete(index)
                                jump_list_box.insert(index, dictin[l])
                            except ValueError:
                                ''
                        breaking = True
                        break
                if breaking:
                    break
            continue
        elif not split_text[i]:
            continue

        if is_in_comments(string_key):
            if split_text[i] == disasm.comments[string_key]:
                continue
        disasm.comments[string_key] = split_text[i]

        if comments_window:
            if key_in_dict(addresses_dict, hex_navi):
                comments_list.delete(addresses_dict[hex_navi])
                if filtering in split_text[i].lower():
                    comments_list.insert(addresses_dict[hex_navi], '{}: {}'.format(hex_navi, split_text[i]))
            else:
                this_int_address = navi << 2
                if disasm.game_address_mode:
                    this_int_address += disasm.game_offset
                target = -1
                for j in range(len(int_addresses)):
                    if this_int_address < int_addresses[j]:
                        target = j
                        break
                if filtering in split_text[i].lower():
                    if target >= 0:
                        comments_list.insert(target, '{}: {}'.format(hex_navi, split_text[i]))
                    else:
                        comments_list.insert(tk.END, '{}: {}'.format(hex_navi, split_text[i]))

        if key_in_dict(orig_keys, hex_navi):
            del jumps_displaying[orig_keys[hex_navi]]
            new_key = orig_keys[hex_navi][:19] + ' ' + disasm.comments[string_key]
            jumps_displaying[new_key] = config[orig_keys[hex_navi]]
            if jumps_window:
                try:
                    index = function_list_box.get(0, tk.END).index(orig_keys[hex_navi])
                    function_list_box.delete(index)
                    function_list_box.insert(index, new_key)
                except ValueError:
                    ''
        breaking = False
        for key in config:
            for l, address in enumerate(config[key]):
                if address[:8] == hex_navi:
                    dictin = config[key]
                    dictin[l] = address[:8] + ' ' + disasm.comments[string_key]
                    if jumps_window:
                        try:
                            index = jump_list_box.get(0, tk.END).index(address)
                            jump_list_box.delete(index)
                            jump_list_box.insert(index, dictin[l])
                        except ValueError:
                            ''
                    breaking = True
                    break
            if breaking:
                break
    save_config()


def buffer_append(buffer):
    if buffer is hack_buffer:
        tuple = (navigation,
                 get_cursor(hack_file_text_box)[0],
                 get_text_content(hack_file_text_box),
                 max_lines,
                 app_config['immediate_identifier'],
                 disasm.game_address_mode,
                 app_config['hex_mode'])
    else:
        tuple = (navigation, get_cursor(comments_text_box)[0], get_text_content(comments_text_box), max_lines)

    # buffer[0] is the current buffer frame being displayed
    # buffer[1] is an array containing the buffer frames
    buffer_length = len(buffer[1])
    distance_from_end = (buffer_length - 1) - buffer[0]

    # This condition means the user is not currently at the end of the buffer (they have done some undo's)
    # so delete all buffer frames following the current one so that the current buffer frame is at the top
    if distance_from_end and buffer_length:
        buffer[1] = buffer[1][:-distance_from_end]
        buffer_length -= distance_from_end
    buffer[1].append(tuple)
    buffer[0] += 1

    # Start shifting buffer frames down and out of the buffer as the limit has been reached
    # Added diff slice in case buffer ever overflows
    diff = buffer_max - buffer[0]
    if diff < 0:
        buffer[0] -= diff
        buffer[1] = buffer[1][diff:]


# Puts the windows clipboard back when the user leaves focus
def replace_clipboard():
    global clipboard
    try:
        window.clipboard_get()
        clipboard = ''
    except:
        if clipboard:
            window.clipboard_append(clipboard)


# Textbox behaviour upon key presses
clipboard = ''
def keyboard_events(handle, max_char, event, buffer = None, hack_function = False):
    global clipboard
    if not disassembler_loaded():
        return
    joined_text = get_text_content(handle)
    split_text = joined_text.split('\n')

    states = {
        str(0b100): 'Control',
        str(0b100000000000000000): 'Alt',
        str(0b1000000000000000000): 'Combines with right-hand Alt or Control',
        str(0b1): 'Shift',
        str(0b1000): 'Num Lock',
        str(0b10): 'Caps Lock',
        str(0b100000): 'Scroll Lock',
    }

    cursor, line, column = get_cursor(handle)
    ctrl_held = bool(event.state & 0b100)
    shift_held = bool(event.state & 0b1)
    alt_held = bool(event.state & 0b100000000000000000)
    up_keysym = event.keysym.upper()
    ctrl_d = ctrl_held and up_keysym == 'D'
    bad_bad_hotkey = ctrl_held and up_keysym in ['Z', 'T']
    if bad_bad_hotkey:
        # Messes with custom behaviour, so wait until after changes are made
        #   by bad hotkey, then restore text box to how it was before the hotkey
        window.after(0, lambda: (handle.delete('1.0', tk.END),
                                 handle.insert('1.0', joined_text),
                                 handle.mark_set(tk.INSERT, cursor),
                                 highlight_stuff()))
        return

    has_char = bool(event.char) and event.keysym != 'Escape' and not ctrl_held
    just_function_key = event.keysym in ['Alt_L', 'Alt_R', 'Shift_L', 'Shift_R', 'Control_L', 'Control_R']
    if not just_function_key:
        reset_target()

    is_cutting = ctrl_held and up_keysym == 'X'
    is_pasting = ctrl_held and up_keysym == 'V'
    is_copying = ctrl_held and up_keysym == 'C'
    is_undoing = buffer and ctrl_held and event.keysym == 'comma'
    is_redoing = buffer and ctrl_held and event.keysym == 'period'
    is_deleting = ctrl_d or event.keysym == 'Delete'
    is_backspacing = event.keysym == 'BackSpace'
    is_returning = event.keysym == 'Return'
    restore_original = up_keysym == 'R' and ctrl_held and hack_function
    insert_branch = up_keysym == 'B' and ctrl_held and hack_function
    wipe_line = ((is_backspacing or is_deleting) and shift_held) or insert_branch

    selection_removable = has_char or is_pasting or is_cutting or is_deleting

    not_arrows = event.keysym not in ['Left', 'Up', 'Right', 'Down']
    vert_arrows = event.keysym in ['Up', 'Down']

    apply_function = apply_hack_changes if hack_function else lambda ignore_slot=None: apply_comment_changes()

    # Cause each modification of text box to snap-shot data in order to undo/redo
    if buffer and ((not (is_undoing or is_redoing) and has_char and not_arrows) or ctrl_d or is_pasting or is_cutting or wipe_line
                   or insert_branch or restore_original):
        buffer_append(buffer)

    # Undoing and Redoing code
    if is_undoing or is_redoing:
        if buffer[0] == len(buffer[1]) - 1 and is_undoing:
            part = buffer[1][buffer[0]]
            if part[0] != navigation or part[2] != joined_text:
                buffer_append(buffer)

        buffer[0] += 1 if is_redoing else -1
        if buffer[0] < 0:
            buffer[0] = 0
        elif buffer[0] > len(buffer[1]) - 1:
            buffer[0] = len(buffer[1]) - 1
        else:
            apply_hack_changes()
            apply_comment_changes()
            place = buffer[0]
            if hack_function:
                immediate_id = buffer[1][place][4]
                game_address_mode = buffer[1][place][5]
                hex_mode = buffer[1][place][6]
                if immediate_id != app_config['immediate_identifier']:
                    app_config['immediate_identifier'] = immediate_id
                    save_config()
                    disasm.immediate_identifier = immediate_id
                if game_address_mode != disasm.game_address_mode:
                    disasm.game_address_mode = game_address_mode
                    save_config()
                if hex_mode != app_config['hex_mode']:
                    app_config['hex_mode'] = hex_mode
                    save_config()
                    disasm.game_address_mode = game_address_mode
            if max_lines != buffer[1][place][3]:
                set_widget_sizes(new_max_lines=buffer[1][place][3])
                app_config['max_lines'] = max_lines
                save_config()

            navigate_to(buffer[1][place][0])
            cursor = buffer[1][place][1]
            text_content = buffer[1][place][2]

            handle.delete('1.0', tk.END)
            handle.insert('1.0', text_content)
            handle.mark_set(tk.INSERT, cursor)

            apply_hack_changes()
            apply_comment_changes()
            reset_target()
            highlight_stuff(skip_moving_cursor=True)
        return

    # Copy/Paste and selection handling
    selection_line_mod = False
    try:
        selection_start, sel_start_line, sel_start_column = get_cursor(handle, tk.SEL_FIRST)
        selection_end, sel_end_line, sel_end_column = get_cursor(handle, tk.SEL_LAST)
        # Select whole columns if selecting multiple lines
        if sel_start_line != sel_end_line:
            # if sel_start_column == len(split_text[sel_start_line - 1]):
            #     selection_line_mod = True
            #     selection_start, sel_start_line, sel_start_column = modify_cursor(selection_start, 1, 0, split_text)
            if sel_end_column == 0:
                selection_line_mod = True
                selection_end, sel_end_line, sel_end_column = modify_cursor(selection_end, -1, 0, split_text)
            selection_start, sel_start_line, sel_start_column = modify_cursor(selection_start, 0, 'min', split_text)
            selection_end, sel_end_line, sel_end_column = modify_cursor(selection_end, 0, 'max', split_text)
    except:
        if is_cutting or is_copying:
            selection_start, sel_start_line, sel_start_column = \
                modify_cursor(cursor, 0, 'min', split_text)
            selection_end, sel_end_line, sel_end_column = \
                modify_cursor(cursor, 0, 'max', split_text)
        else:
            selection_start, sel_start_line, sel_start_column = '1.0', 0, 0
            selection_end, sel_end_line, sel_end_column = '1.0', 0, 0

    has_selection = selection_start != selection_end
    selection_lines = sel_end_line - sel_start_line
    selection_function = has_selection and (selection_removable or is_copying)
    standard_key = not is_backspacing and not is_returning and has_char
    temp_cursor, _, __ = modify_cursor(selection_start, 0, -1, split_text)
    lower_outer_bound_selection_char = handle.get(temp_cursor)
    upper_outer_bound_selection_char = handle.get(selection_end)
    pasting_newlines = '\n' in clipboard
    paste_text = ''
    lines_diff = 0

    if (wipe_line or (not has_selection and is_pasting and not pasting_newlines)) and hack_function:
        handle.delete(cursor_value(line, 0), cursor_value(line, len(split_text[line-1])))
        new_column = 0
        if insert_branch:
            branch_template = 'BEQ R0, R0, {}'.format(disasm.immediate_identifier)
            handle.insert(cursor_value(line, 0), branch_template)
            new_column = len(branch_template)
        window.after(0, lambda: handle.mark_set(tk.INSERT, cursor_value(line, new_column)))


    # Because using mark_set() on SEL_FIRST or SEL_LAST seems to corrupt the widgets beyond repair at a windows level,
    # A work around with a custom clipboard is required in order for the code to be able to serve it's intended purpose
    if has_selection and not selection_lines:
        if is_pasting and pasting_newlines:
            selection_start, sel_start_line, sel_start_column = modify_cursor(selection_start, 0, 'min', split_text)
            selection_end, sel_end_line, sel_end_column = modify_cursor(selection_end, 0, 'max', split_text)
        if is_deleting:
            selection_end, sel_end_line, sel_end_column = modify_cursor(selection_end, 0, -1, split_text)
        if is_backspacing:
            selection_start, sel_start_line, sel_start_column = modify_cursor(selection_start, 0, 1, split_text)

    if selection_function:
        selection_text = handle.get(selection_start, selection_end)

        if is_copying or is_cutting:
            clipboard = selection_text
            # So that when user pastes, window clipboard won't override clipboard
            window.after(0, window.clipboard_clear)

        if selection_removable:
            handle.delete(selection_start, selection_end)
            lines_diff += selection_lines
            handle.mark_set(tk.INSERT, selection_start)

    if is_pasting:
        # If window clipboard has contents, contents to be drawn from there, else draw from clipboard
        # window clipboard having contents means user has copied data from an external source
        try:
            winnie_clip = window.clipboard_get()
            clipboard = winnie_clip
            window.clipboard_clear()
        except:
            winnie_clip = clipboard
        if winnie_clip:
            if '\n' in winnie_clip:
                # Ensure the text has within the maximum amount of lines and columns
                split_clip = winnie_clip.split('\n')
                line_boundary = max_lines - (sel_start_line if has_selection else line)
                split_clip = split_clip[:line_boundary + 1]
                split_clip = [i[:max_char] for i in split_clip]
                lines_diff -= len(split_clip) - 1
                winnie_clip = '\n'.join(split_clip)
                if not selection_function:
                    min_del, _, __ = modify_cursor(cursor, 0, 'min', split_text)
                    max_del, _, __ = modify_cursor(cursor, -lines_diff, 'max', split_text)
                    handle.delete(min_del, max_del)
                    lines_diff = 0
            paste_text = winnie_clip

    # Either clear lines which would be excess lines after the paste
    # Or add new lines to fill in what would be the gaps after the paste
    insertion_place = selection_start if has_selection else cursor
    if lines_diff > 0:
        handle.insert(insertion_place, '\n' * lines_diff)
    elif lines_diff < 0:
        temp_cursor, _, __ = modify_cursor(insertion_place, -lines_diff, 'max', split_text)
        handle.delete(insertion_place, temp_cursor)

    if is_pasting or is_cutting:
        def move_next(handle):
            temp_cursor, _, __ = modify_cursor(handle.index(tk.INSERT), 0, 'max', get_text_content(handle))
            handle.mark_set(tk.INSERT, temp_cursor)
        handle.insert(insertion_place, paste_text)
        if not selection_line_mod or is_pasting:
            window.after(1, lambda: (apply_hack_changes(),
                                     apply_comment_changes(),
                                     move_next(handle),
                                     navigate_to(navigation)))
    # Copy/Paste end

    # Easier than recalculating for each condition in the copy/paste section
    cursor, line, column = get_cursor(handle)
    # selection_start, sel_start_line, sel_start_column, selection_end, sel_end_line, sel_end_column = selection_calc()
    # has_selection = selection_start != selection_end
    if selection_removable:
        selection_start, sel_start_line, sel_start_column, selection_end, sel_end_line, sel_end_column = 0,0,0,0,0,0
        has_selection = False
    joined_text = get_text_content(handle)
    split_text = joined_text.split('\n')

    # The strange way I have gone about this is so that jumps and branches are automatically mapped or unmapped by
    #   apply_hack_changes(), and also to account for both hex and assembly mode.
    if restore_original:
        base_text = get_text_content(base_file_text_box).split('\n')
        start = line if not sel_start_line else sel_start_line
        end = line if not sel_start_line else sel_end_line
        curs_1 = cursor_value(start, 0)
        curs_2 = cursor_value(end, len(split_text[end - 1]))
        replace_text = base_text[start-1:end]
        to_fix = []
        for i in range(len(replace_text)):
            if replace_text[i] == 'UNKNOWN/NOT AN INSTRUCTION':
                to_fix.append((start - 1) + i + navigation)
                replace_text[i] = 'NOP'
        handle.delete(curs_1, curs_2)
        handle.insert(curs_1, '\n'.join(replace_text))
        apply_hack_changes()
        # print(get_text_content(hack_file_text_box))
        for i in to_fix:
            navi = i << 2
            disasm.split_and_store_bytes(int_of_4_byte_aligned_region(disasm.base_file[navi:navi+4]), i)
        navigate_to(navigation)
        return

    nl_at_cursor = handle.get(cursor) == '\n'
    if has_selection:
        nl_at_cursor = nl_at_cursor or handle.get(selection_end) == '\n'
    # Make any key delete the final character of the line if word is about to wrap onto next line
    # Also validate all code except line currently editing
    if standard_key:
        apply_function(ignore_slot = line - 1)
        if split_text[line - 1] == 'NOP' and hack_function:
            handle.delete(cursor_value(line, 0), cursor_value(line, 3))
        line_chars = len(split_text[line - 1])
        if line_chars > max_char - 1:
            handle.delete(cursor_value(line, max_char - 1), cursor_value(line, max_char))

    # Make delete do nothing if cursor precedes a new line
    # Make backspace act as left arrow if cursor at column 0 then validate code (ignoring the line if cursor not at column 0)
    elif ((is_backspacing and (column == 0 and line > 1)) or (is_deleting and nl_at_cursor and not shift_held)) and not has_selection:
        # if not selection_lines: # was needed to stop something but now is not?
        if is_deleting:
            # apply_function(ignore_slot = (line - 1) if not sel_start_line else None)
            window.after(0, lambda: apply_function(ignore_slot = (line - 1) if not sel_start_line else None))
        handle.insert(cursor,'\n')
        handle.mark_set(tk.INSERT, cursor)
        if is_backspacing:
            window.after(0, lambda: apply_function(ignore_slot = (line - 1) if not sel_start_line else None))
            # apply_function(ignore_slot = (line - 1) if not sel_start_line else None)

    # Make return send the cursor to the end of the next line and validate code
    elif is_returning:
        move_lines = -1 if line == max_lines else 0
        cursor, _, __ = modify_cursor(cursor, move_lines, 'max', split_text)
        handle.mark_set(tk.INSERT, cursor)
        handle.delete(cursor)
        new_cursor, _, __ = modify_cursor(cursor, 1 if not shift_held else -1, 'max', split_text)
        window.after(0, lambda: (apply_function(), handle.mark_set(tk.INSERT, new_cursor)))

    cursor, line, column = get_cursor(handle)
    split_text = get_text_content(handle).split('\n')

    # Prevent delete or backspace from modifying textbox any further than the bounds of the selected text (if selected text is only on one line)
    if has_selection and not selection_lines:
        if (is_deleting and column != len(split_text[line - 1])) or (is_backspacing and column != 0):
            replace_char = lower_outer_bound_selection_char if is_backspacing else upper_outer_bound_selection_char
        elif (is_backspacing and column == 0) or (is_deleting and sel_end_column == len(split_text[sel_end_line - 1])):
            replace_char = '\n'
        else:
            replace_char = ''
        if not (is_pasting or is_returning):
            handle.insert(selection_start, replace_char)
            window.after(0, lambda: apply_function())
        if is_deleting:
            window.after(0, lambda: handle.mark_set(tk.INSERT, selection_start))

    # So the P on the cursor's NOP doesn't get removed when backspace happens
    elif has_selection and selection_lines and is_backspacing:
        handle.insert(cursor, 'P')

    if vert_arrows:
        apply_function()

    if selection_removable and selection_line_mod and (standard_key or is_cutting):
        def move_to():
            temp_cursor = get_cursor(handle)[0]
            handle.insert(temp_cursor, '\n')
            handle.mark_set(tk.INSERT, temp_cursor)
            apply_function()

        window.after(0, move_to)

    # The delays are necessary to solve complications for text modified by the key after this function fires
    if not just_function_key:
        if handle is comments_text_box:
            window.after(0, lambda: (apply_comment_changes(), highlight_stuff(event.widget)))
        else:
            window.after(0, lambda: (highlight_stuff(event.widget)))


base_file_text_box.bind('<Key>', lambda event:
    keyboard_events(base_file_text_box, 30, event, buffer=False))

hack_file_text_box.bind('<Key>', lambda event:
    keyboard_events(hack_file_text_box, 30, event, buffer=hack_buffer, hack_function=True))

comments_text_box.bind('<Key>', lambda event:
    keyboard_events(comments_text_box, 70, event, buffer=comments_buffer))


# The button is destroyed and remade every time the user scrolls within it's view
change_rom_name_button = tk.Button()
def change_rom_name():
    if not disassembler_loaded():
        return
    new_name = simpledialog.askstring('Change rom name', '20 Characters maximum')
    if new_name:
        if len(new_name) < 20:
            new_name += ' ' * (20 - len(new_name))
        elif len(new_name) > 20:
            new_name = new_name[:20]
        # new_name = new_name.upper()
        for i in range(20):
            disasm.hack_file[disasm.header_items['Rom Name'][0] + i] = ord(new_name[i])
        disasm.comments['9'] = new_name
        navigate_to(navigation)


def destroy_change_rom_name_button():
    global change_rom_name_button
    if change_rom_name_button:
        change_rom_name_button.destroy()
        change_rom_name_button = None


def navigate_to(index, center=False, widget=None):
    global navigation, change_rom_name_button, prev_cursor_location
    if not disassembler_loaded():
        return
    destroy_change_rom_name_button()
    indexed = index
    if center:
        index -= max_lines >> 1
    shift_amount = navigation

    # Correct the navigation if traveling out of bounds, also calculate limits for file samples to display
    amount_words = disasm.file_length // 4
    navigation = index if index + max_lines < amount_words else amount_words - max_lines
    if navigation < 0:
        navigation = 0
    limit = navigation + max_lines if navigation + max_lines < amount_words else amount_words
    lines = limit - navigation
    if center:
        prev_cursor_location = indexed

    shift_amount -= navigation

    # Sample bytes out of files
    file_nav = navigation * 4
    base_sample = disasm.base_file[file_nav:file_nav + ((limit if limit < max_lines else max_lines) * 4)]
    hack_sample = disasm.hack_file[file_nav:file_nav + ((limit if limit < max_lines else max_lines) * 4)]

    # Translate each 4 lot of bytes into separate integers
    ints_in_base_sample = ints_of_4_byte_aligned_region(base_sample)
    ints_in_hack_sample = ints_of_4_byte_aligned_region(hack_sample)

    # Create blank comments box, then fill with any comments that have been made (we don't store any blank comments)
    sample_comments = [''] * lines
    for i in range(lines):
        string_key = '{}'.format(navigation + i)
        if string_key in disasm.comments.keys():
            sample_comments[i] = disasm.comments[string_key]

    # Calculate what addresses to display in the address box, and disassemble ints into instructions, display header section as hex
    address_range = [extend_zeroes(hexi((i * 4) + (disasm.game_offset
                                                   if disasm.game_address_mode else 0)), 8) for i in range(navigation, limit)]

    base_disassembled = [disasm.decode(ints_in_base_sample[i], navigation + i)
                         if navigation + i > 15 and not app_config['hex_mode'] else
                         hex_space(extend_zeroes(hexi(ints_in_base_sample[i]), 8))
                         for i in range(len(ints_in_base_sample))]
    hack_disassembled = [disasm.decode(ints_in_hack_sample[i], navigation + i)
                         if navigation + i > 15 and not app_config['hex_mode'] else
                         hex_space(extend_zeroes(hexi(ints_in_hack_sample[i]), 8))
                         for i in range(len(ints_in_hack_sample))]

    # Replace disassembled data in the hack file with any errors the user has made
    for i in range(len(hack_disassembled)):
        if not hack_disassembled[i]:
            hack_disassembled[i] = 'UNKNOWN/NOT AN INSTRUCTION'
        if not base_disassembled[i]:
            base_disassembled[i] = 'UNKNOWN/NOT AN INSTRUCTION'
        string_key = '{}'.format(navigation + i)
        if string_key in user_errors.keys():
            hack_disassembled[i] = user_errors[string_key][1]

    # Display floating Rom Name Change button
    if disasm.header_items['Rom Name'][0] // 4 in range(navigation, limit):
        bg, fg = app_config['text_bg_colour'], app_config['text_fg_colour']
        change_rom_name_button = tk.Button(window, text = 'Change', command = change_rom_name,
                                           bg=bg, fg=fg,
                                           activebackground=bg, activeforeground=fg)
        c_w, c_h, c_x, c_y = geometry(comments_text_box.winfo_geometry())
        font_w, font_h = font_dimension(main_font_size)
        x_offset = (font_w * 22) + 5
        y_offset = ((disasm.header_items['Rom Name'][0] // 4) - navigation) * font_h
        change_rom_name_button.place(x = c_x + x_offset, y = 36 + y_offset, height = 20)

    # Update all 4 text boxes
    def update_text_box(handle, text):
        text = '\n'.join(text)
        cursor, line, column = get_cursor(handle)
        handle.delete('1.0', tk.END)
        handle.insert('1.0', text)

        new_line = line + shift_amount
        if new_line < 1 or new_line > max_lines:
            new_cursor_loc = cursor_value(keep_within(new_line, 1, max_lines), 0)
        else:
            new_cursor_loc, _, __ = modify_cursor(cursor, shift_amount, 0, text)

        handle.mark_set(tk.INSERT, new_cursor_loc)

    params = [[address_text_box, address_range],
              [base_file_text_box, base_disassembled],
              [hack_file_text_box, hack_disassembled],
              [comments_text_box, sample_comments]]
    [update_text_box(param[0], param[1]) for param in params]

    if center:
        widgey = widget if widget else hack_file_text_box
        new_cursor = modify_cursor('1.0', max_lines >> 1, 'max', get_text_content(widgey))[0]
        widgey.mark_set(tk.INSERT, new_cursor)
    elif prev_cursor_location in range(navigation, limit):
        line = prev_cursor_location - navigation
        new_cursor, _, __ = modify_cursor('1.0', line, 'max', get_text_content(hack_file_text_box))
        hack_file_text_box.mark_set(tk.INSERT, new_cursor)

    highlight_stuff(widget, skip_moving_cursor=center)


def navigation_callback(address):
    widget = check_widget(window.focus_get())
    if not address and widget:
        widget.focus_force()
    try:
        address = deci(address)
        if disasm.game_address_mode:
            address -= disasm.game_offset
        address //= 4
    except:
        if widget:
            widget.focus_force()
        return
    apply_hack_changes()
    apply_comment_changes()
    reset_target()
    navigate_to(address, center=True, widget=widget)
    if widget:
        widget.focus_force()



def navigation_prompt(root=window):
    if not disassembler_loaded():
        return
    address = simpledialog.askstring('Navigate to address', '', parent=root)
    navigation_callback(address)


def scroll_callback(event):
    if not disassembler_loaded():
        return
    apply_hack_changes()
    apply_comment_changes()
    direction = -app_config['scroll_amount'] if event.delta > 0 else app_config['scroll_amount']
    navigate_to(navigation + direction, widget=check_widget(window.focus_get()))


def save_changes_to_file(save_as=False):
    if not disassembler_loaded():
        return False

    apply_hack_changes()
    apply_comment_changes()

    # Do not save changes if there are errors
    for key in user_errors:
        navigate_to(int(key), widget=hack_file_text_box, center=True)
        return False
    status_text.set('Calculating checksum...')
    window.update()
    if app_config['calc_crc'][disasm.hack_file_name]:
        sum1, sum2 = disasm.calc_checksum()
    else:
        sum1 = sum2 = 0
    if app_config['calc_crc'][disasm.hack_file_name] and navigation <= disasm.header_items['CRC2'][0] >> 2:
        navigate_to(navigation)
    if save_as:
        new_file_name = filedialog.asksaveasfilename(initialdir = app_config['previous_hack_location'],
                                                     title = 'Save as...')
        if not new_file_name:
            return False
        new_file_path = os.path.realpath(new_file_name)
        if new_file_path == disasm.base_folder + disasm.base_file_name:
            simpledialog.messagebox._show('Wait a sec', 'You shouldn\'t select the base file')
            return False
        new_file_name = new_file_path[new_file_path.rfind('\\') + 1:]
        if not '.' in new_file_name:
            dot = disasm.hack_file_name.rfind('.')
            new_file_name += disasm.hack_file_name[dot:]
        new_dir = new_file_path[:new_file_path.rfind('\\') + 1]
        new_file_path = new_dir + new_file_name
        if exists(new_file_path):
            simpledialog.messagebox._show('Sorry', 'That file already exists.')
            return False
        app_config['previous_hack_location'] = new_dir
        app_config['previous_hack_opened'] = new_file_path
        app_config['hack_of_base'][new_file_name] = app_config['hack_of_base'][disasm.hack_file_name]
        app_config['calc_crc'][new_file_name] = app_config['calc_crc'][disasm.hack_file_name]
        disasm.hack_file_name = new_file_name
        disasm.comments_file = new_file_path + ' comments.txt'
        disasm.jumps_file = new_file_path + ' jumps.data'

        window.title('ROM Disassembler - ' + disasm.hack_file_name)

    app_config['CIC'][disasm.hack_file_name] = disasm.cic
    app_config['jumps_displaying'][disasm.hack_file_name] = jumps_displaying.copy()
    app_config['game_address_mode'][disasm.hack_file_name] = disasm.game_address_mode
    save_config()
    with open(disasm.jumps_file, 'wb') as jumps_file:
        dump((disasm.jumps_to, disasm.branches_to), jumps_file)

    with open(disasm.hack_folder + disasm.hack_file_name, 'wb') as file:
        file.write(disasm.hack_file)

    if exists(disasm.comments_file):
        with open(disasm.comments_file + '(Backup).txt', 'w') as backup_comments_file:
            with open(disasm.comments_file, 'r') as comments_file:
                backup_comments_file.write(comments_file.read())

    try:
        with open(disasm.comments_file, 'w') as file:
            file.write(dict_to_string(disasm.comments))
        if exists(disasm.comments_file + '(Backup).txt'):
            os.remove(disasm.comments_file + '(Backup).txt')
    except Exception as e:
        simpledialog.messagebox._show('Error', 'There was trouble saving your comments file. '
                                               'A backup of your old comments can be found next to the original comments file. '
                                               'Your rom file was saved without error. '
                                               '\nYou should go there now and rescue the backup before attempting to save again.'
                                               '\nIf you save again, that backup will be over-written.'
                                               '\n\n' + str(e))
    checksum_text = ' Checksum calculated - CRC1: {} | CRC2: {}'.format(
            extend_zeroes(hexi(sum1), 8),
            extend_zeroes(hexi(sum2), 8))
    message = 'Rom Saved.'
    if app_config['calc_crc'][disasm.hack_file_name]:
        message += checksum_text
    status_text.set(message)
    window.update()
    return True


def destroy_them(not_main=False):
    global colours_window, jumps_window, comments_window, dimension_window, manual_cic_win
    global changes_win, opcodes_win
    if changes_win:
        changes_win.destroy()
        changes_win = None
    if colours_window:
        colours_window.destroy()
        colours_window = None
    if jumps_window:
        jumps_window.destroy()
        jumps_window = None
    if comments_window:
        comments_window.destroy()
        comments_window = None
    if dimension_window:
        dimension_window.destroy()
        dimension_window = None
    if manual_cic_win:
        manual_cic_win.destroy()
        manual_cic_win = None
    if opcodes_win:
        opcodes_win.destroy()
        opcodes_win = None
    if not not_main:
        window.destroy()


def close_window(side = 'right'):
    if not disassembler_loaded():
        destroy_them()
        return

    close_win_width = 270
    close_win_height = 45
    close_win_y_offset = 130

    win_w, win_h, win_x, win_y = geometry(window.geometry())

    placement_x = ((win_w if side == 'right' else close_win_width)  + win_x) - close_win_width
    placement_y = (close_win_y_offset + win_y) - close_win_height

    close_win_geo = '{}x{}+{}+{}'.format(close_win_width, close_win_height, placement_x, placement_y)

    close_win = tk.Tk()
    close_win.geometry(close_win_geo)
    close_win.title('Exit')
    label = tk.Label(close_win, text = 'Save work?').place(x = 150, y = 12)

    def yes_button_callback():
        if save_changes_to_file():
            destroy_them()
        close_win.destroy()

    yes_button = tk.Button(close_win, text='Yes',command = yes_button_callback)
    no_button = tk.Button(close_win, text='No',command = lambda:\
        (destroy_them(), close_win.destroy()))

    yes_button.place(x=10, y=10, width=50)
    no_button.place(x=75, y=10, width=50)

    def cancel_close_win():
        close_win.destroy()

    close_win.protocol('WM_DELETE_WINDOW', cancel_close_win)
    close_win.bind('<FocusOut>', lambda _: close_win.destroy())
    close_win.focus_force()
    close_win.mainloop()


def open_files(mode = ''):
    global disasm, jumps_displaying

    if disassembler_loaded():
        if not save_changes_to_file():
            return
        disasm = None
        jumps_displaying = {}
        reset_target()
        target_of_down_label.place_forget()
        target_of_up_label.place_forget()
        [text_box.delete('1.0', tk.END) for text_box in ALL_TEXT_BOXES]
        window.title('ROM Disassembler')
    [text_box.configure(state=tk.NORMAL) for text_box in ALL_TEXT_BOXES]
    destroy_change_rom_name_button()
    destroy_them(not_main=True)

    # Set data for rest of this function
    if mode == 'new':
        base_title = 'Select the original base rom'
        hack_title = 'Choose location and name for the new hacked rom'
        hack_dialog_function = filedialog.asksaveasfilename
    else:
        base_title = 'Select the base rom'
        hack_title = 'Select the hacked rom'
        hack_dialog_function = filedialog.askopenfilename

    base_dir = ''
    base_file_path = ''
    # Obtain file locations from user input
    if app_config['open_roms_automatically'] and app_config['previous_base_opened'] and not mode:
        base_file_path = app_config['previous_base_opened']
    if mode == 'new':
        base_file_path = filedialog.askopenfilename(initialdir = app_config['previous_base_location'], title = base_title)
        if not base_file_path:
            return
        base_file_path = os.path.realpath(base_file_path)
        base_dir = base_file_path[:base_file_path.rfind('\\') + 1]

    hack_dir = base_dir if mode == 'new' else app_config['previous_hack_location']
    if app_config['open_roms_automatically'] and app_config['previous_hack_opened'] and not mode:
        hack_file_path = app_config['previous_hack_opened']
    else:
        hack_file_path = hack_dialog_function(initialdir = hack_dir, title = hack_title)
    if not hack_file_path:
        return
    hack_file_path = os.path.realpath(hack_file_path)
    hack_dir = hack_file_path[:hack_file_path.rfind('\\') + 1]
    if mode == 'existing':
        hack_name = hack_file_path[hack_file_path.rfind('\\') + 1:]
        if hack_name not in app_config['hack_of_base']:
            app_config['hack_of_base'][hack_name] = ''
        if not exists(app_config['hack_of_base'][hack_name]):
            base_file_path = filedialog.askopenfilename(initialdir=hack_dir, title=
                                                        'There is no associated base rom. Select it now.')
            if not base_file_path:
                return
            base_file_path = os.path.realpath(base_file_path)
            if base_file_path == hack_file_path:
                simpledialog.messagebox._show('Wait a sec', 'You shouldn\'t choose the same files.')
                return
        else:
            base_file_path = app_config['hack_of_base'][hack_name]
        base_dir = base_file_path[:base_file_path.rfind('\\') + 1]
    base_dot = base_file_path.rfind('.')
    file_extension = base_file_path[base_dot + 1:]
    if not '.' in hack_file_path[hack_file_path.rfind('\\'):]:
        hack_file_path += '.' + file_extension

    if mode == 'new':
        if os.path.exists(hack_file_path):
            simpledialog.messagebox._show('Sorry', 'That file already exists')
            return
        else:
            with open(base_file_path, 'rb') as base_file:
                with open(hack_file_path, 'wb') as hack_file:
                    hack_file.write(base_file.read())

    timer_reset()

    # Remember dirs for next browse
    app_config['previous_base_location'] = base_dir
    app_config['previous_hack_location'] = hack_dir
    save_config()

    # Initialise disassembler with paths to the 2 files, apply saved settings from app_config
    try:
        disasm = Disassembler(base_file_path,
                              hack_file_path,
                              window,
                              status_text)
        if disasm.hack_file_name not in app_config['game_address_mode']:
            app_config['game_address_mode'][disasm.hack_file_name] = False
        disasm.game_address_mode = app_config['game_address_mode'][disasm.hack_file_name]
        disasm.immediate_identifier = app_config['immediate_identifier']

    except Exception as e:
        simpledialog.messagebox._show('Error', e)
        base_file_text_box.delete('1.0', tk.END)
        hack_file_text_box.delete('1.0', tk.END)
        [text_box.config(state=tk.DISABLED) for text_box in ALL_TEXT_BOXES]
        disasm = None
        return

    app_config['hack_of_base'][disasm.hack_file_name] = \
        app_config['previous_base_opened'] = disasm.base_folder + disasm.base_file_name
    app_config['previous_hack_opened'] = disasm.hack_folder + disasm.hack_file_name

    save_config()
    if disasm.hack_file_name not in app_config['CIC']:
        bne_1 = 'BNE A3, T0'
        bne_2 = 'BNE S0, T0'
        places = [0x670, 0x66C, 0x63C, 0x77C]
        cics = ['6101', '6102', '6103', '6105']
        new_cic = '6105'
        for i in range(4):
            navi_1, navi_2 = places[i], places[i] + 12
            place_1 = navi_1 >> 2
            place_2 = navi_2 >> 2
            inst_1 = disasm.decode(int_of_4_byte_aligned_region(disasm.base_file[navi_1:navi_1+4]), place_1)
            inst_2 = disasm.decode(int_of_4_byte_aligned_region(disasm.base_file[navi_2:navi_2+4]), place_2)
            if bne_1 == inst_1[:10] and bne_2 == inst_2[:10]:
                if cics[i] == '6101':
                    simpledialog.messagebox._show('Ahh..', 'I\'ve been waiting for you, starfox.\n\n For this game, '
                                                           'you may or may not need to bypass the CRC (in the tools menu).')
                else:
                    new_cic = cics[i]
                break
            if i == 3:
                load_instruction = lambda i: disasm.decode(int_of_4_byte_aligned_region(disasm.hack_file[i:i+4]), i >> 2)
                targ_instructions = {
                    '40': 'MTC0 R0, CAUSE',
                    '180': 'SW V0, $0000 (SP)',
                    '8F4': 'MFLO K0',
                    'A8C': 'BNE A1, K1, $00000A9C'
                }
                is_6106 = True
                for i in targ_instructions:
                    navi = deci(i)
                    inst = load_instruction(navi)
                    if inst != targ_instructions[i]:
                        is_6106 = False
                if is_6106:
                    new_cic = '6106'
                else:
                    simpledialog.messagebox._show('Warning', 'Could not determine CIC chip. '
                                                             'Defaulting to most common chip.\n'
                                                             'Rom may get stuck in infinite loop '
                                                             'while booting. If the rom won\'t boot, '
                                                             'try bypassing the CRC with "Tools->'
                                                             'Bypass CRC".')
        app_config['CIC'][disasm.hack_file_name] = CIC[new_cic]
        app_config['calc_crc'][disasm.hack_file_name] = True
    calc_crc.set(app_config['calc_crc'][disasm.hack_file_name])
    disasm.set_cic(app_config['CIC'][disasm.hack_file_name])
    hack_file_text_box.insert('1.0', 'Mapping jumps and branches...\nPlease wait...')
    comments_text_box.insert('1.0', 'This may take a while with larger roms.\nThis only has to be done once per rom.\n\n'
                                    'If your window is not responding - don\'t worry.')
    window.update()
    def rest_of_function():
        global jumps_displaying
        window.title('ROM Disassembler - ' + disasm.hack_file_name)
        disasm.map_jumps(address_text_box)
        [text_box.delete('1.0',tk.END) for text_box in ALL_TEXT_BOXES]
        window.update()
        disasm.loaded = True

        # Navigate user to first line of code, start the undo buffer with the current data on screen
        navigate_to(0)
        buffer_append(hack_buffer)
        buffer_append(comments_buffer)
        if disasm.hack_file_name not in app_config['jumps_displaying']:
            app_config['jumps_displaying'][disasm.hack_file_name] = {}
        jumps_displaying = app_config['jumps_displaying'][disasm.hack_file_name].copy()
        time_taken = timer_get()
        if time_taken > 100:
            simpledialog.messagebox._show('Woah!', 'That took {} seconds.'.format(time_taken))

    # Otherwise text boxes sometimes don't get updated to notify user of jump mapping
    window.after(1, rest_of_function)


def toggle_address_mode():
    global function_select
    if not disassembler_loaded():
        return
    apply_hack_changes()
    apply_comment_changes()
    cursor, line, column = get_cursor(hack_file_text_box)
    buffer_append(hack_buffer)
    toggle_to = not disasm.game_address_mode
    disasm.game_address_mode = toggle_to
    if disassembler_loaded():
        disasm.game_address_mode = toggle_to
    save_config()
    navigate_to(navigation)
    hack_file_text_box.mark_set(tk.INSERT, cursor)
    highlight_stuff(skip_moving_cursor=True)
    increment = disasm.game_offset if toggle_to else -disasm.game_offset
    config_data = jumps_displaying.copy()

    def fix_listbox(listbox):
        list_contents = listbox.get(0, tk.END)
        addresses = [extend_zeroes(hexi(deci(i[:8]) + increment), 8) for i in list_contents]
        listbox.delete(0, tk.END)
        for i, address in enumerate(addresses):
            listbox.insert(tk.END, '{}{}'.format(address, list_contents[i][8:]))

    if comments_window:
        fix_listbox(comments_list)

    if changes_win:
        fix_listbox(changes_list_box)

    for key in config_data:
        del jumps_displaying[key]
        addy_1 = deci(key[:8]) + increment
        addy_2 = deci(key[11:19]) + increment
        comment = key[19:]
        addy_1 = extend_zeroes(hexi(addy_1), 8)
        addy_2 = extend_zeroes(hexi(addy_2), 8)
        new_key = '{} - {}{}'.format(addy_1, addy_2, comment)
        jumps_displaying[new_key] = []
        for address in config_data[key]:
            new_address = extend_zeroes(hexi(deci(address[:8]) + increment), 8)
            jumps_displaying[new_key].append(new_address + address[8:])
    save_config()
    if jumps_window:
        if function_select:
            addy_1 = function_select[:8]
            addy_2 = function_select[11:19]
            comment = function_select[19:]
            addy_1 = extend_zeroes(hexi(deci(addy_1) + increment), 8)
            addy_2 = extend_zeroes(hexi(deci(addy_2) + increment), 8)
            function_select = '{} - {}{}'.format(addy_1, addy_2, comment)
        function_list_box.delete(0, tk.END)
        jump_box = jump_list_box.get(0,tk.END)
        jump_list_box.delete(0, tk.END)
        for key in jumps_displaying:
            function_list_box.insert(tk.END, key)
        for i in jump_box:
            value = extend_zeroes(hexi(deci(i[:8]) + increment), 8)
            jump_list_box.insert(tk.END, value + i[8:])


def toggle_hex_mode():
    apply_hack_changes()
    apply_comment_changes()
    cursor, line, column = get_cursor(hack_file_text_box)
    buffer_append(hack_buffer)
    app_config['hex_mode'] = not app_config['hex_mode']
    save_config()
    navigate_to(navigation)
    hack_file_text_box.mark_set(tk.INSERT, cursor)
    highlight_stuff(skip_moving_cursor=True)


def toggle_hex_space():
    cursor, line, column = get_cursor(hack_file_text_box)
    app_config['hex_space_separation'] = not app_config['hex_space_separation']
    save_config()
    navigate_to(navigation)
    hack_file_text_box.mark_set(tk.INSERT, cursor)
    highlight_stuff(skip_moving_cursor=True)



def change_immediate_id():
    accepted_symbols = ['<', '>', ':', ';', '\'', '"', '{', '}',
                        '=', '+', '-', '_', '*', '&', '^', '%', '$', '#',
                        '@', '!', '`', '~', '/', '?', '\\']
    symbol = simpledialog.askstring('Set immediate identifier symbol',
                                    'Must be one of {}'.format(' '.join(accepted_symbols)))
    if symbol and symbol[:1] in accepted_symbols:
        apply_hack_changes()
        apply_comment_changes()
        buffer_append(hack_buffer)
        for key in user_errors:
            user_errors[key] = user_errors[key].replace(app_config['immediate_identifier'], symbol[:1])
        app_config['immediate_identifier'] = symbol[:1]
        if disassembler_loaded():
            disasm.immediate_identifier = symbol[:1]
        navigate_to(navigation)
        buffer_append(hack_buffer)
        save_config()


def set_scroll_amount():
    amount = simpledialog.askstring('Set scroll amount', 'Current: {} lines'.format(app_config['scroll_amount']) +
                                                         '\nUse 0x to specify an amount of bytes')
    try:
        amount = deci(amount) >> 2 if amount[:2] == '0x' else int(amount)
    except:
        return
    app_config['scroll_amount'] = amount
    save_config()


def help_box():
    message = '\n'.join([
        '----General Info----',
        'The base rom file is never modified, even if you try to make modifications to the textbox. '
        'It is simply there to reflect on if you need to see the original code at any point.',
        '',
        'Once you have decided the FILE name (not inner rom title) for your hacked rom, it is best not '
        'to ever change it. If you wish to rename the output rom file, you can load your project as '
        'you normally would and then "save as..." to specify a new name so that all of the app data '
        'pertaining to that rom will migrate over to the newly named rom.',
        '',
        'In order to save any changes you have made, all errors must be corrected before the save feature will allow it. '
        'Trying to save while an error exists will result in your navigation shifting to the next error instead.',
        '',
        'The comments file will be output to where your hacked rom is located. '
        'The comments file must always be located in the same folder as your hacked rom in order for it to load. '
        'You can also open the comments files with a text editor if required.',
        '',
        'Clicking the address list on the left side will cause the address you clicked to be copied to your '
        'clipboard automatically.',
        '',
        '----Translate Address----',
        'The "Translate Address" section is for addresses you find with your memory editor to be translated to the corresponding '
        'memory address for your emulator. In order to use this feature you will have to grab the game entry point address from '
        'your memory editor and paste it into "Tools->Set memory editor offset". After this, you may use the text box on the left '
        'side of the translate button to translate addresses. You may copy an address in to automatically have it translated, '
        'or alternatively, you may press the translate button and your clipboard contents will automatically be converted. '
        'When auto copy output to clipboard is on, every time an address is translated your clipboard will be replaced with the output.'

    ])
    message_2 = '\n'.join([
        '----Highlighting----',
        'There are different coloured highlights for jumps/branches and more. See "Window->Change Colour Scheme" '
        'for more details',
        '',
        '',
        '----General Hotkeys----',
        'Ctrl+S: Quick save',
        'F1: Open comments browser',
        'F2: Open jumps currently memorised',
        'F3: Toggle textbox containing original code',
        'F4: Navigate to address',
        'F5: Toggle mode which displays and handles addresses using the game\'s entry point',
        'F6: Toggle hex mode',
        'F7: Toggle byte separation during hex mode',
        '',
        '',
        '----Jumps window----',
        'Click on any instruction inside the function (within the hack text box) that you wish to find all jumps to '
        'and press Ctrl+G. '
        'This will open your jumps window and from there you can use it to navigate around all jumps to the function. '
        'If no functions show up when you press Ctrl+G, then there aren\'t any hardcoded jumps to that function. Be on the lookout '
        'for the highlighted instructions for "Targets of any Jump" in your colour scheme menu. '
        'The functions in the jump window will be memorised until you delete them. '
        'You can delete functions from memory by selecting it in the jumps window and '
        'pressing the delete key. '
        'Adding a comment to the very top of the function (or the jump to the function) using the comments text box will '
        'cause it to become labeled as such within the jumps window. '
        'The mapped jumps are only mapped by a basic "distance from UNKNOWN/NOT AN INSTRUCTION" algorithm, so there may be some '
        'miss-decoded jumps from sections that aren\'t instructions, and very few unmapped actual jumps.'
    ])
    imm_id = '$'
    if disassembler_loaded():
        imm_id = disasm.immediate_identifier
    message_3 = '\n'.join([
        '----Hack Textbox Hotkeys/Info----',
        'Ctrl+F: Follow jump/branch at text insert cursor',
        'Ctrl+G: Find all jumps to function enveloping text insert cursor',
        'Ctrl+R: Restore multi-line selection or line at text insert cursor to original code',
        'Ctrl+B: Replace current line with "BEQ R0, R0, {}"'.format(imm_id),
        'Shift+Delete or Shift+Backspace: Remove line of text at text insert cursor',
        'Return: Move to end of next line',
        'Shift+Return: Move to end of previous line',
        'Ctrl+Comma: Undo',
        'Ctrl+Fullstop: Redo',
        '',
        'For the sake of a 100% accurate decoding process and performance, no pseudo-instructions '
        'were able to be implemented.',
        '',
        'When making a multi-line selection, you don\'t need to worry about highlighting '
        'the entirety of the start and end column, that will be done automatically when you attempt '
        'to action your selection.',
        '',
        'When pasting in multiple lines of code, it will fit it to the correct lines in the text box. '
        'If the amount of code you paste extends beyond the bottom of the text box, it will be cut to fit it.',
        '',
        'Any blank line will be replaced with NOP. Sometimes it may not happen immediately, but this '
        'is not a problem.',
        '',
        'Any NOP instruction will automatically be typed over if you attempt to type on that line.',
        '',
        'Shift+Delete/Shift+Backspace, Return, Shift+Return, Undo and Redo can be used on the comments '
        'textbox as well as the hack textbox.',
        '',
        'The hacked rom text box and comments text box have separate undo/redo buffers. '
        'Both buffers can hold up to 20,000  frames each.',
        '',
        'The selection highlighting may seem buggy at times, but it causes no harm. I had to write a '
        'work-around to override the default highlighting software because the default methods it '
        'provides programmers to modify the text selection were just plain not working.'
    ])
    simpledialog.messagebox._show('Help', message)
    simpledialog.messagebox._show('Help (continued)', message_2)
    simpledialog.messagebox._show('Help (final)', message_3)


opcodes_win = None
def opcodes_list():
    global opcodes_win
    opcodes_win = tk.Tk()
    opcodes_win.title('Opcodes list')
    opcodes_win.geometry('650x800+50+50')
    codes_list = tk.Listbox(opcodes_win, font=('Courier', 10))
    [codes_list.insert(tk.END, i + ': ' + DOCUMENTATION[i]) for i in DOCUMENTATION]
    codes_list.place(x=5, y=5, width=640, height=790)
    opcodes_win.bind('<Escape>', lambda _: opcodes_win.destroy())
    opcodes_win.mainloop()


def about_box():

    def callback(event):
        webbrowser.open_new('https://github.com/mikeryan/n64dev/tree/master/docs/n64ops')

    about = tk.Tk()
    about.title('About')
    message = tk.Label(about, text='Made by Mitchell Parry-Shaw'
                                   '\nDisassembler created using data from:')
    link = tk.Label(about, text='https://github.com/mikeryan/n64dev/tree/master/docs/n64ops', fg="blue", cursor="hand2")
    message.pack()
    link.pack(side=tk.RIGHT)
    link.bind("<Button-1>", callback)
    about.focus_force()
    about.bind('<FocusOut>', lambda e: about.destroy())
    about.mainloop()


def is_in_comments(key):
    try:
        _ = disasm.comments[key]
        return True
    except:
        return False


jumps_window = None
function_list_box = None
jump_list_box = None
jumps_label = None
function_select = ''
jumps_displaying = {}
def find_jumps(just_window=False):
    global function_select, jumps_window, function_list_box, jump_list_box, jumps_label
    if not disassembler_loaded():
        return
    if just_window:
        cursor, line, column = '1.0', 1, 0
    else:
        cursor, line, column = get_cursor(hack_file_text_box)
    navi = (line - 1) + navigation
    if not just_window:
        jumps, function_start, function_end = disasm.find_jumps(navi)
    else:
        jumps, function_start, function_end = 0,0,0

    if not jumps_window:
        jumps_window = tk.Tk()
        jumps_window.title('Jumps to Functions')
        jumps_window.geometry('{}x{}'.format(jumps_win_w,jumps_win_h))
        jumps_window.bind('<F5>', lambda e: toggle_address_mode())
        function_list_box = tk.Listbox(jumps_window, font=('Courier', main_font_size))
        jump_list_box = tk.Listbox(jumps_window, font=('Courier', main_font_size))
        def hack_checkbox_callback():
            if app_config['jumps_auto_focus_hack']:
                app_config['jumps_auto_focus_hack'] = False
                hack_checkbox.deselect()
            else:
                app_config['jumps_auto_focus_comments'] = False
                app_config['jumps_auto_focus_hack'] = True
                hack_checkbox.select()
                comment_checkbox.deselect()
            save_config()

        def comments_checkbox_callback():
            if app_config['jumps_auto_focus_comments']:
                app_config['jumps_auto_focus_comments'] = False
                comment_checkbox.deselect()
            else:
                app_config['jumps_auto_focus_hack'] = False
                app_config['jumps_auto_focus_comments'] = True
                comment_checkbox.select()
                hack_checkbox.deselect()
            save_config()

        hack_checkbox = tk.Checkbutton(jumps_window, text='Auto-focus hack textbox',
                                       command=lambda: window.after(1, lambda: hack_checkbox_callback()))
        comment_checkbox = tk.Checkbutton(jumps_window, text='Auto-focus comments textbox',
                                          command=lambda: window.after(1, lambda: comments_checkbox_callback()))
        hack_checkbox.place(x=100, y=6)
        comment_checkbox.place(x=294, y=6)
        if app_config['jumps_auto_focus_comments']:
            comment_checkbox.select()
        elif app_config['jumps_auto_focus_hack']:
            hack_checkbox.select()

        def function_list_callback():
            global function_select
            curselect = function_list_box.curselection()
            if not curselect:
                return
            apply_hack_changes()
            apply_comment_changes()
            key = function_list_box.get(curselect[0])
            function_select = key
            increment = 0 if not disasm.game_address_mode else -(disasm.game_offset >> 2)
            start_address = deci(key[:8]) >> 2
            reset_target()
            widget = None
            if app_config['jumps_auto_focus_comments']:
                widget = comments_text_box
            elif app_config['jumps_auto_focus_hack']:
                widget = hack_file_text_box
            navigate_to(start_address + increment, center=True, widget=widget)
            jump_list_box.delete(0, tk.END)
            for address in jumps_displaying[key]:
                jump_list_box.insert(tk.END, address)
            if widget:
                widget.focus_force()

        def function_list_key(event):
            if event.keysym == 'Delete':
                curselect = function_list_box.curselection()
                if curselect:
                    try:
                        del jumps_displaying[function_list_box.get(curselect[0])]
                        save_config()
                    finally:
                        function_list_box.delete(curselect[0])
                        jump_list_box.delete(0, tk.END)

        def jump_list_callback():
            curselect = jump_list_box.curselection()
            if not curselect:
                return
            apply_hack_changes()
            apply_comment_changes()
            increment = 0 if not disasm.game_address_mode else -(disasm.game_offset >> 2)
            address = jump_list_box.get(curselect[0])[:8]
            navi = (deci(address) >> 2) + increment
            reset_target()
            widget = None
            if app_config['jumps_auto_focus_comments']:
                widget = comments_text_box
            elif app_config['jumps_auto_focus_hack']:
                widget = hack_file_text_box
            navigate_to(navi, center=True, widget=widget)
            if widget:
                widget.focus_force()

        function_list_box.bind('<<ListboxSelect>>', lambda _: function_list_callback())
        jump_list_box.bind('<<ListboxSelect>>', lambda _: jump_list_callback())
        function_list_box.bind('<Key>', function_list_key)
        for key in jumps_displaying:
            function_list_box.insert(tk.END,key)
        tk.Label(jumps_window, text='Functions').place(x=6,y=5)
        jumps_label = tk.Label(jumps_window, text='Jumps to Function')
        function_list_box.place(x=func_list_x,y=func_list_y,width=func_list_w,height=func_list_h)
        jump_list_box.place(x=jumps_list_x,y=jumps_list_y,width=jumps_list_w,height=jumps_list_h)
        jumps_label.place(x=jumps_list_x,y=jumps_label_y)
        def jumps_window_equals_none():
            global jumps_window, function_select
            jumps_window.destroy()
            function_select = ''
            jumps_window = None
        jumps_window.protocol('WM_DELETE_WINDOW', jumps_window_equals_none)
        jumps_window.bind('<Escape>', lambda e: jumps_window_equals_none())
        jumps_window.bind('<F1>', lambda e: view_comments())
        jumps_window.bind('<F3>', lambda e: toggle_base_file())
        jumps_window.bind('<F4>', lambda e: navigation_prompt(root=jumps_window))
        jumps_window.bind('<F5>', lambda e: toggle_address_mode())
        jumps_window.bind('<Control-s>', lambda e: save_changes_to_file())
        jumps_window.bind('<Control-S>', lambda e: save_changes_to_file())
        jumps_window.focus_force()
        jumps_window.after(1, lambda: jumps_window.mainloop())
    elif jumps or just_window:
        jumps_window.focus_force()
    if just_window:
        return
    key = extend_zeroes(hexi(function_start << 2),8) + ' - ' + extend_zeroes(hexi(function_end << 2),8)
    key_not_in_jumps_displaying = True
    config = jumps_displaying.copy()
    increment = -disasm.game_offset if disasm.game_address_mode else 0
    try:
        comment_key = str((deci(key[:8]) + increment) >> 2)
    except ValueError:
        # User attempting to get jumps from the top of the header
        return
    for display_key in config:
        key_not_in_jumps_displaying = key_not_in_jumps_displaying and display_key[:19] != key
        if not key_not_in_jumps_displaying:
            if is_in_comments(comment_key):
                new_key = key + ' ' + disasm.comments[comment_key]
            else:
                new_key = key
            if new_key != display_key:
                del jumps_displaying[display_key]
                key_not_in_jumps_displaying = True
                key = new_key
    if not config:
        if is_in_comments(comment_key):
            key += ' {}'.format(disasm.comments[comment_key])
    if key_not_in_jumps_displaying and jumps:
        for i in range(len(jumps)):
            comment_key = str((deci(jumps[i]) + increment) >> 2)
            if is_in_comments(comment_key):
                jumps[i] += ' ' + disasm.comments[comment_key]
        jumps_displaying[key] = jumps
        save_config()
        if function_list_box:
            function_list_box.insert(tk.END, key)


comments_window = None
comments_list = None
filter_text = None
def view_comments():
    global comments_window, comments_list, filter_text
    if not disassembler_loaded():
        return
    if comments_window:
        comments_window.focus_force()
        return
    comments_window = tk.Tk()
    comments_window.title('Comments')
    comments_window.geometry('{}x{}'.format(comments_win_w,comments_win_h))
    comments_list = tk.Listbox(comments_window, font=('Courier', main_font_size))
    comments_list.place(x=comments_x,y=comments_y,width=comments_w,height=comments_h)

    # If this doesn't want to work the way the auto-copy checkbox does, then it will have to work by force
    def hack_checkbox_callback():
        if app_config['comments_auto_focus_hack']:
            app_config['comments_auto_focus_hack'] = False
            hack_checkbox.deselect()
        else:
            app_config['comments_auto_focus_comments'] = False
            app_config['comments_auto_focus_hack'] = True
            hack_checkbox.select()
            comment_checkbox.deselect()
        save_config()

    def comments_checkbox_callback():
        if app_config['comments_auto_focus_comments']:
            app_config['comments_auto_focus_comments'] = False
            comment_checkbox.deselect()
        else:
            app_config['comments_auto_focus_hack'] = False
            app_config['comments_auto_focus_comments'] = True
            comment_checkbox.select()
            hack_checkbox.deselect()
        save_config()

    def increment():
        if disasm.game_address_mode:
            return disasm.game_offset
        return 0

    def populate_list():
        if comments_window:
            comments_list.delete(0, tk.END)
            filtering = filter_text.get('1.0', tk.END).split('\n')[0].lower()
            for key in sorted([int(key) for key in disasm.comments if filtering in disasm.comments[key].lower()]):
                comments_list.insert(tk.END, '{}: {}'.format(extend_zeroes(hexi((key << 2) + increment()), 8),
                                                             disasm.comments[str(key)]))

    hack_checkbox = tk.Checkbutton(comments_window, text='Auto-focus hack textbox',
                                   command=lambda: window.after(1, lambda: hack_checkbox_callback()))
    comment_checkbox = tk.Checkbutton(comments_window, text='Auto-focus comments textbox',
                                      command=lambda: window.after(1, lambda: comments_checkbox_callback()))
    filter_text = tk.Text(comments_window, font=('Courier', 10))
    hack_checkbox.place(x=6, y=6)
    comment_checkbox.place(x=175, y=6)
    filter_text.place(x=370, y=9, width=font_dimension(10)[0] * 20, height=20)
    tk.Label(comments_window, text='Filter list').place(x=535,y=9)
    filter_text.bind('<Key>', lambda _: window.after(1, populate_list))
    if app_config['comments_auto_focus_comments']:
        comment_checkbox.select()
    elif app_config['comments_auto_focus_hack']:
        hack_checkbox.select()

    populate_list()

    def comments_list_callback(event):
        curselect = comments_list.curselection()
        if not curselect:
            return
        apply_hack_changes()
        apply_comment_changes()
        navi = deci(comments_list.get(curselect[0])[:8]) >> 2
        if disasm.game_address_mode:
            navi -= disasm.game_offset >> 2
        reset_target()
        widget = None
        if app_config['comments_auto_focus_comments']:
            widget = comments_text_box
        elif app_config['comments_auto_focus_hack']:
            widget = hack_file_text_box
        navigate_to(navi, center=True, widget=widget)
        if widget:
            widget.focus_force()

    comments_list.bind('<<ListboxSelect>>', comments_list_callback)

    def comments_window_equals_none():
        global comments_window
        comments_window.destroy()
        comments_window = None

    comments_window.bind('<Escape>', lambda e: comments_window_equals_none())
    comments_window.bind('<F2>', lambda e: find_jumps(just_window=True))
    comments_window.bind('<F3>', lambda e: toggle_base_file())
    comments_window.bind('<F4>', lambda e: navigation_prompt(root=comments_window))
    comments_window.bind('<F5>', lambda e: toggle_address_mode())
    comments_window.bind('<Control-s>', lambda e: save_changes_to_file())
    comments_window.bind('<Control-S>', lambda e: save_changes_to_file())
    comments_window.protocol('WM_DELETE_WINDOW', comments_window_equals_none)
    comments_window.focus_force()
    comments_window.mainloop()


def follow_jump():
    if not disassembler_loaded():
        return
    cursor, line, column = get_cursor(hack_file_text_box)
    navi = (line - 1) + navigation
    navi_4 = navi << 2
    int_word = ints_of_4_byte_aligned_region(disasm.hack_file[navi_4: navi_4 + 4])[0]
    opcode = (int_word & 0xFC000000) >> 26
    navi += 1  # Address calculated based on address of delay slot
    address = 0
    if JUMP_INTS[opcode]:
        address = disasm.solve_address(navi, int_word & 0x03FFFFFF) >> 2
    elif BRANCH_INTS[opcode]:
        address = sign_16_bit_value(int_word & 0xFFFF) + navi
    if address:
        text_box_contents = get_text_content(hack_file_text_box)
        # (navigation, cursor_location, text_box_content, immediate_id, game_address_mode)
        frame = hack_buffer[1][hack_buffer[0]]
        if frame[0] != navigation or frame[1] != cursor or frame[2] != text_box_contents:
            buffer_append(hack_buffer)
        navigate_to(address, center=True)
        buffer_append(hack_buffer)


colours_window = None

def set_colour_scheme():
    global colours_window
    if colours_window:
        return
    if not disassembler_loaded():
        simpledialog.messagebox._show('Please note', 'You will need to open rom files in order to see your text '
                                                     'colour changes.')
    colours_window = tk.Tk()
    colours_window.title('Colour scheme')
    colours_window.geometry('{}x{}'.format(458, 486))

    def colour_buttons_callback(which, with_colour=''):
        if not with_colour:
            if which in app_config['tag_config']:
                start_colour = app_config['tag_config'][which]
            else:
                start_colour = app_config[which]
            new_colour = colorchooser.askcolor(color=start_colour)
        else:
            r, g, b = get_colours_of_hex(with_colour)
            new_colour = ((r, g, b), with_colour)
        if not new_colour[0]:
            return
        if which in app_config['tag_config']:
            app_config['tag_config'][which] = new_colour[1]
        else:
            app_config[which] = new_colour[1]
        grey_scale = (new_colour[0][0] + new_colour[0][1] + new_colour[0][2]) / 3
        if grey_scale < 128:
            fg = '#FFFFFF'
        else:
            fg = '#000000'
        bg = new_colour[1]
        custom_buttons[which].config(bg=bg, activebackground=bg, fg=fg, activeforeground=fg)
        change_colours()
        save_config()

    button_text = {
        'text_bg_colour': 'Textbox background',
        'text_fg_colour': 'Text colour',
        'window_background_colour': 'Window background',
        'cursor_line_colour': 'Input cursor line',
        'liken': 'Selected register',
        'target': 'Target of selected Jump/Branch',
        'jump_from': 'Jumps/Branches to selected instruction',
        'jump_to': 'Targets of any Jump',
        'branch_to': 'Targets of any Branch',
        'branch': 'Branch Instructions',
        'jump': 'Jump Instructions',
        'nop': 'NOP',
        'function_end': 'JR RA',
        'bad': 'Syntax errors',
        'out_of_range': 'Immediate out of range errors',
    }
    current_colours = {}
    for i in button_text:
        if i in app_config['tag_config']:
            current_colours[i] = app_config['tag_config'][i]
        else:
            current_colours[i] = app_config[i]
    dark_colours = {}
    for i in button_text:
        if i in FRESH_APP_CONFIG['tag_config']:
            dark_colours[i] = FRESH_APP_CONFIG['tag_config'][i]
        else:
            dark_colours[i] = FRESH_APP_CONFIG[i]
    bright_colours = {
        'text_bg_colour': '#ffffff',
        'text_fg_colour': '#000000',
        'window_background_colour': '#e8e8e8',
        'cursor_line_colour': '#ebebeb',
        'liken': '#1be9b0',
        'target': '#35ffe1',
        'jump_from': '#f9c5bf',
        'branch_to': '#c6fff2',
        'jump_to': '#c9d9ff',
        'branch': '#f37a7a',
        'jump': '#f89789',
        'nop': '#d8d8d8',
        'function_end': '#ca88e6',
        'bad': '#dd3333',
        'out_of_range': '#be2323'
    }
    custom_buttons = {}
    previous_setting_buttons = {}
    default_dark_buttons = {}
    default_bright_buttons = {}
    for tag in button_text:
        custom_buttons[tag] = tk.Button(colours_window)
        previous_setting_buttons[tag] = tk.Button(colours_window)
        default_bright_buttons[tag] = tk.Button(colours_window)
        default_dark_buttons[tag] = tk.Button(colours_window)

    # For some reason, I cannot set the command inside a loop. It sets all callbacks with 'out_of_range', the final iteration
    custom_buttons['text_bg_colour'].config(command=lambda: colour_buttons_callback('text_bg_colour'))
    custom_buttons['text_fg_colour'].config(command=lambda: colour_buttons_callback('text_fg_colour'))
    custom_buttons['window_background_colour'].config(command=lambda: colour_buttons_callback('window_background_colour'))
    custom_buttons['cursor_line_colour'].config(command=lambda: colour_buttons_callback('cursor_line_colour'))
    custom_buttons['branch_to'].config(command=lambda: colour_buttons_callback('branch_to'))
    custom_buttons['jump_to'].config(command=lambda: colour_buttons_callback('jump_to'))
    custom_buttons['jump_from'].config(command=lambda: colour_buttons_callback('jump_from'))
    custom_buttons['branch'].config(command=lambda: colour_buttons_callback('branch'))
    custom_buttons['jump'].config(command=lambda: colour_buttons_callback('jump'))
    custom_buttons['liken'].config(command=lambda: colour_buttons_callback('liken'))
    custom_buttons['target'].config(command=lambda: colour_buttons_callback('target'))
    custom_buttons['nop'].config(command=lambda: colour_buttons_callback('nop'))
    custom_buttons['function_end'].config(command=lambda: colour_buttons_callback('function_end'))
    custom_buttons['bad'].config(command=lambda: colour_buttons_callback('bad'))
    custom_buttons['out_of_range'].config(command=lambda: colour_buttons_callback('out_of_range'))
    previous_setting_buttons['text_bg_colour'].config(command=lambda: colour_buttons_callback('text_bg_colour',current_colours['text_bg_colour']))
    previous_setting_buttons['text_fg_colour'].config(command=lambda: colour_buttons_callback('text_fg_colour',current_colours['text_fg_colour']))
    previous_setting_buttons['window_background_colour'].config(command=lambda: colour_buttons_callback('window_background_colour',current_colours['window_background_colour']))
    previous_setting_buttons['cursor_line_colour'].config(command=lambda: colour_buttons_callback('cursor_line_colour',current_colours['cursor_line_colour']))
    previous_setting_buttons['branch_to'].config(command=lambda: colour_buttons_callback('branch_to',current_colours['branch_to']))
    previous_setting_buttons['jump_to'].config(command=lambda: colour_buttons_callback('jump_to',current_colours['jump_to']))
    previous_setting_buttons['jump_from'].config(command=lambda: colour_buttons_callback('jump_from',current_colours['jump_from']))
    previous_setting_buttons['branch'].config(command=lambda: colour_buttons_callback('branch',current_colours['branch']))
    previous_setting_buttons['jump'].config(command=lambda: colour_buttons_callback('jump',current_colours['jump']))
    previous_setting_buttons['liken'].config(command=lambda: colour_buttons_callback('liken',current_colours['liken']))
    previous_setting_buttons['target'].config(command=lambda: colour_buttons_callback('target',current_colours['target']))
    previous_setting_buttons['nop'].config(command=lambda: colour_buttons_callback('nop',current_colours['nop']))
    previous_setting_buttons['function_end'].config(command=lambda: colour_buttons_callback('function_end',current_colours['function_end']))
    previous_setting_buttons['bad'].config(command=lambda: colour_buttons_callback('bad',current_colours['bad']))
    previous_setting_buttons['out_of_range'].config(command=lambda: colour_buttons_callback('out_of_range',current_colours['out_of_range']))
    default_dark_buttons['text_bg_colour'].config(command=lambda: colour_buttons_callback('text_bg_colour',dark_colours['text_bg_colour']))
    default_dark_buttons['text_fg_colour'].config(command=lambda: colour_buttons_callback('text_fg_colour',dark_colours['text_fg_colour']))
    default_dark_buttons['window_background_colour'].config(command=lambda: colour_buttons_callback('window_background_colour',dark_colours['window_background_colour']))
    default_dark_buttons['cursor_line_colour'].config(command=lambda: colour_buttons_callback('cursor_line_colour',dark_colours['cursor_line_colour']))
    default_dark_buttons['branch_to'].config(command=lambda: colour_buttons_callback('branch_to',dark_colours['branch_to']))
    default_dark_buttons['jump_to'].config(command=lambda: colour_buttons_callback('jump_to',dark_colours['jump_to']))
    default_dark_buttons['jump_from'].config(command=lambda: colour_buttons_callback('jump_from',dark_colours['jump_from']))
    default_dark_buttons['branch'].config(command=lambda: colour_buttons_callback('branch',dark_colours['branch']))
    default_dark_buttons['jump'].config(command=lambda: colour_buttons_callback('jump',dark_colours['jump']))
    default_dark_buttons['liken'].config(command=lambda: colour_buttons_callback('liken',dark_colours['liken']))
    default_dark_buttons['target'].config(command=lambda: colour_buttons_callback('target',dark_colours['target']))
    default_dark_buttons['nop'].config(command=lambda: colour_buttons_callback('nop',dark_colours['nop']))
    default_dark_buttons['function_end'].config(command=lambda: colour_buttons_callback('function_end',dark_colours['function_end']))
    default_dark_buttons['bad'].config(command=lambda: colour_buttons_callback('bad',dark_colours['bad']))
    default_dark_buttons['out_of_range'].config(command=lambda: colour_buttons_callback('out_of_range',dark_colours['out_of_range']))
    default_bright_buttons['text_bg_colour'].config(command=lambda: colour_buttons_callback('text_bg_colour',bright_colours['text_bg_colour']))
    default_bright_buttons['text_fg_colour'].config(command=lambda: colour_buttons_callback('text_fg_colour',bright_colours['text_fg_colour']))
    default_bright_buttons['window_background_colour'].config(command=lambda: colour_buttons_callback('window_background_colour',bright_colours['window_background_colour']))
    default_bright_buttons['cursor_line_colour'].config(command=lambda: colour_buttons_callback('cursor_line_colour',bright_colours['cursor_line_colour']))
    default_bright_buttons['branch_to'].config(command=lambda: colour_buttons_callback('branch_to',bright_colours['branch_to']))
    default_bright_buttons['jump_to'].config(command=lambda: colour_buttons_callback('jump_to',bright_colours['jump_to']))
    default_bright_buttons['jump_from'].config(command=lambda: colour_buttons_callback('jump_from',bright_colours['jump_from']))
    default_bright_buttons['branch'].config(command=lambda: colour_buttons_callback('branch',bright_colours['branch']))
    default_bright_buttons['jump'].config(command=lambda: colour_buttons_callback('jump',bright_colours['jump']))
    default_bright_buttons['liken'].config(command=lambda: colour_buttons_callback('liken',bright_colours['liken']))
    default_bright_buttons['target'].config(command=lambda: colour_buttons_callback('target',bright_colours['target']))
    default_bright_buttons['nop'].config(command=lambda: colour_buttons_callback('nop',bright_colours['nop']))
    default_bright_buttons['function_end'].config(command=lambda: colour_buttons_callback('function_end',bright_colours['function_end']))
    default_bright_buttons['bad'].config(command=lambda: colour_buttons_callback('bad',bright_colours['bad']))
    default_bright_buttons['out_of_range'].config(command=lambda: colour_buttons_callback('out_of_range',bright_colours['out_of_range']))

    def change_all(colour_dict):
        [colour_buttons_callback(tag, colour_dict[tag]) for tag in colour_dict]

    y_offset = 0
    for button in custom_buttons:
        if button in app_config['tag_config']:
            bg = app_config['tag_config'][button]
        else:
            bg = app_config[button]
        int_of_bg = int('0x' + bg[1:], 16)
        grey_scale = (((int_of_bg & 0xFF0000) >> 16) + ((int_of_bg & 0xFF00) >> 8) + (int_of_bg & 0xFF)) / 3
        if grey_scale < 128:
            fg = '#FFFFFF'
        else:
            fg = '#000000'
        tk.Label(colours_window, text='Customise').place(x=85,y=10)
        custom_buttons[button].config(text=button_text[button],
                                      bg=bg, activebackground=bg, fg=fg, activeforeground=fg)
        custom_buttons[button].place(x=5, y=35+y_offset, width=222)

        default_dark_buttons[button].config(bg=dark_colours[button], activebackground=dark_colours[button])
        default_dark_buttons[button].place(x=396, y=35+y_offset, width=50)

        default_bright_buttons[button].config(bg=bright_colours[button], activebackground=bright_colours[button])
        default_bright_buttons[button].place(x=319, y=35+y_offset, width=50)

        previous_setting_buttons[button].config(bg=current_colours[button], activebackground=current_colours[button])
        previous_setting_buttons[button].place(x=242, y=35+y_offset, width=50)
        y_offset += 30

    c_bg, c_fg = current_colours['text_bg_colour'], current_colours['text_fg_colour']
    tk.Button(colours_window, text='Current', bg=c_bg, fg=c_fg, activebackground=c_bg, activeforeground=c_fg,
              command=lambda:change_all(current_colours)).place(x=242,y=5,width=50)

    b_bg, b_fg = bright_colours['text_bg_colour'], bright_colours['text_fg_colour']
    tk.Button(colours_window, text='Bright', bg=b_bg, fg=b_fg, activebackground=b_bg, activeforeground=b_fg,
              command=lambda:change_all(bright_colours)).place(x=319,y=5,width=50)

    d_bg, d_fg = dark_colours['text_bg_colour'], dark_colours['text_fg_colour']
    tk.Button(colours_window, text='Dark', bg=d_bg, fg=d_fg, activebackground=d_bg, activeforeground=d_fg,
              command=lambda:change_all(dark_colours)).place(x=396, y=5,width=50)

    def close_colours_win():
        global colours_window
        colours_window.destroy()
        colours_window = None
    colours_window.bind('<Escape>', lambda e: close_colours_win())
    colours_window.protocol('WM_DELETE_WINDOW', close_colours_win)
    colours_window.mainloop()


def set_mem_edit_offset():
    if not disassembler_loaded():
        return
    user_input = simpledialog.askstring('','Set game offset for memory editor')
    if not user_input:
        return
    try:
        user_input = deci(user_input)
        app_config['mem_edit_offset'][disasm.hack_file_name] = user_input
        save_config()
    except:
        simpledialog.messagebox._show('Error', 'Not able to convert input to a number.')


def translate_box(button=False):
    if not disassembler_loaded():
        return
    address_output.delete('1.0', tk.END)
    address_to_translate = address_input.get('1.0',tk.END).replace('\n','')
    if not address_to_translate and button:
        try:
            address_to_translate = window.clipboard_get()
        except:
            return
    try:
        if disasm.hack_file_name not in app_config['mem_edit_offset']:
            simpledialog.messagebox._show('Warning','You have not set your memory editor offset yet.')
            raise Exception()
        int_address = deci(address_to_translate)
        mem_offset = app_config['mem_edit_offset'][disasm.hack_file_name]
        output_text = extend_zeroes(hexi((int_address - mem_offset) + disasm.game_offset + 0x1000), 8)
        address_output.insert('1.0', output_text)
        if auto_copy_var.get():
            window.clipboard_clear()
            window.clipboard_append(output_text)
            status_text.set('Copied {} to clipboard'.format(output_text))
    except:
        address_output.insert('1.0', 'Error')
    finally:
        address_input.delete('1.0', tk.END)


def toggle_auto_copy():
    app_config['auto_copy'] = auto_copy_var.get()
    save_config()

def toggle_auto_open():
    toggle_to = not app_config['open_roms_automatically']
    app_config['open_roms_automatically'] = toggle_to
    save_config()

def target_up():
    target_down_label.place_forget()
    target_up_label.place(x=top_label_x, y=top_label_y, width=top_label_w, height=8)


def target_down():
    target_up_label.place_forget()
    target_down_label.place(x=bot_label_x, y=bot_label_y, width=bot_label_w, height=8)


def target_none():
    target_up_label.place_forget()
    target_down_label.place_forget()


targ_direction = 0
def target_of_up():
    global targ_direction
    targ_direction = (targ_direction & 0b10) + 0b01
    target_of_up_label.place(x=targ_top_label_x, y=targ_top_label_y,
                             width=targ_top_label_w, height=8)


def target_of_down():
    global targ_direction
    targ_direction = (targ_direction & 0b01) + 0b10
    target_of_down_label.place(x=targ_bot_label_x, y=targ_bot_label_y,
                             width=targ_bot_label_w, height=8)


def toggle_base_file():
    app_config['toggle_base_file'] = not app_config['toggle_base_file']
    apply_comment_changes()
    apply_hack_changes()
    navigate_to(navigation)
    save_config()
    set_widget_sizes()


def set_widget_sizes(new_size=main_font_size, new_max_lines=max_lines):
    global main_font_size, max_lines, top_label_x, top_label_w, bot_label_x, bot_label_w
    global top_label_y, bot_label_y, comments_win_h, comments_x, comments_y, comments_w
    global comments_h, jumps_win_w, jumps_win_h, func_list_w, func_list_y, func_list_x
    global func_list_h, jumps_list_x, jumps_list_y, jumps_list_w, jumps_list_h, jumps_label
    global jumps_label_y, comments_win_w, targ_top_label_x, targ_top_label_y, targ_top_label_w
    global targ_bot_label_x, targ_bot_label_y, targ_bot_label_w
    if disassembler_loaded():
        apply_comment_changes()
        apply_hack_changes()
    window.update_idletasks()
    main_font_size = new_size
    max_lines = new_max_lines
    font_w, font_h = font_dimension(main_font_size)
    widget_y = 35
    widget_h = (max_lines * font_h) + 4
    win_w, win_h, win_x, win_y = geometry(window.geometry())
    x_1 = 6
    w_1 = (font_w * 8) + 6
    w_2 = (font_w * 30) + 6
    if app_config['toggle_base_file']:
        x_2 = x_1 + w_1 + 4
        x_3 = x_2 + w_2 + 4
    else:
        x_2 = 0
        x_3 = x_1 + w_1 + 4
    x_4 = x_3 + w_2 + 4
    w_4 = (font_w * 70) + 6
    if app_config['status_bar']:
        status_bar_size = font_h + 8
    else:
        status_bar_size = 0
    win_w = x_4 + w_4 + 5
    win_h = status_bar_size + widget_h + widget_y + 5
    top_label_x = bot_label_x = x_3 + 2
    top_label_w = bot_label_w = w_2 - 5
    targ_top_label_y = top_label_y = widget_y - 4
    targ_bot_label_y = bot_label_y = (widget_y + widget_h) - 5
    targ_bot_label_x = targ_top_label_x = top_label_x + 50
    targ_bot_label_w = targ_top_label_w = top_label_w - 100
    [text_box.config(font=('Courier', main_font_size)) for text_box in ALL_TEXT_BOXES + [status_bar]]
    address_text_box.place(x=x_1, y=widget_y, width=w_1, height=widget_h)
    if x_2:
        base_file_text_box.place(x=x_2, y=widget_y, width=w_2, height=widget_h)
    else:
        base_file_text_box.place_forget()
    hack_file_text_box.place(x=x_3, y=widget_y, width=w_2, height=widget_h)
    comments_text_box.place(x=x_4, y=widget_y, width=w_4, height=widget_h)
    window.geometry('{}x{}+{}+{}'.format(win_w, win_h, win_x, win_y))
    if disassembler_loaded():
        # Or the clumsy change rom name button's placement suffers a race condition and uses previous settings
        window.after(1, lambda: navigate_to(navigation))
    comments_x = 6
    comments_y = 35
    comments_w = (font_w * 80) + 6
    comments_h = font_h * 25
    comments_win_w = comments_x + comments_w + 5
    comments_win_h = comments_y + comments_h + 5
    if comments_window:
        _, __, comments_win_x, comments_win_y = geometry(comments_window.geometry())
        comments_list.config(font=('Courier', main_font_size))
        comments_list.place(x=comments_x, y=comments_y, width=comments_w, heigh=comments_h)
        comments_window.geometry('{}x{}+{}+{}'.format(comments_win_w, comments_win_h, comments_win_x, comments_win_y))
    func_list_x = jumps_list_x = 6
    func_list_w = jumps_list_w = (font_w * 91) + 6
    func_list_h = jumps_list_h = font_h * 10
    func_list_y = 35
    jumps_label_y = func_list_y + func_list_h + 5
    jumps_list_y = jumps_label_y + 30
    jumps_win_w = func_list_w + func_list_x + 5
    jumps_win_h = jumps_list_y + jumps_list_h + 5
    if jumps_window:
        _, __, jumps_win_x, jumps_win_y = geometry(jumps_window.geometry())
        [i.config(font=('Courier', main_font_size)) for i in [jump_list_box, function_list_box]]
        jump_list_box.place(x=jumps_list_x, y=jumps_list_y, width=jumps_list_w, height=jumps_list_h)
        function_list_box.place(x=func_list_x, y=func_list_y, width=func_list_w, height=func_list_h)
        jumps_label.place(x=jumps_list_x, y=jumps_label_y)
        jumps_window.geometry('{}x{}+{}+{}'.format(jumps_win_w, jumps_win_h, jumps_win_x, jumps_win_y))


def toggle_status_bar():
    app_config['status_bar'] = not app_config['status_bar']
    save_config()
    if app_config['status_bar']:
        status_bar.pack(side=tk.BOTTOM, fill=tk.X, padx=5, pady=3)
    else:
        status_bar.pack_forget()
    set_widget_sizes()


dimension_window = None
def change_win_dimensions():
    global dimension_window
    if dimension_window:
        dimension_window.focus_force()
        return
    dimension_window = tk.Tk()
    dimension_window.title('Change window dimensions')
    def scale_callback():
        new_size, new_lines = scale_size.get(), scale_lines.get()
        set_widget_sizes(new_size, new_lines)
    scale_size = tk.Scale(dimension_window, from_=4, to=30, command=lambda _: scale_callback())
    scale_lines = tk.Scale(dimension_window, from_=1, to=80, command=lambda _: scale_callback())
    [scale.config(orient=tk.HORIZONTAL) for scale in [scale_size, scale_lines]]
    scale_size.set(app_config['font_size'])
    scale_lines.set(app_config['max_lines'])
    def dimension_scroll_callback(event):
        shift = 1 if event.delta > 0 else -1
        if event.widget is scale_size:
            scale_size.set(keep_within(scale_size.get() + shift, 4, 30))
        elif event.widget is scale_lines:
            scale_lines.set(keep_within(scale_lines.get() + shift, 1, 120))

    def dimension_window_equals_none():
        global dimension_window
        app_config['font_size'] = scale_size.get()
        app_config['max_lines'] = scale_lines.get()
        dimension_window.destroy()
        dimension_window = None
        save_config()
    tk.Label(dimension_window, text='Font size').pack(side=tk.LEFT)
    scale_size.pack(side=tk.LEFT)
    tk.Label(dimension_window, text='Line amount').pack(side=tk.RIGHT)
    scale_lines.pack(side=tk.RIGHT)
    dimension_window.bind('<MouseWheel>', dimension_scroll_callback)
    dimension_window.bind('<Escape>', lambda e: dimension_window_equals_none())
    dimension_window.protocol('WM_DELETE_WINDOW', dimension_window_equals_none)
    dimension_window.focus_force()
    dimension_window.mainloop()


def bypass_crc():
    if not disassembler_loaded():
        return
    j, j3, decode, decode3 = disasm.find_checksum_loop()
    if j:
        disasm.split_and_store_bytes(0, j)
        disasm.split_and_store_bytes(0, j + 3)
        app_config['calc_crc'][disasm.hack_file_name] = False
        calc_crc.set(False)
        save_config()
        disasm.comments[str(j)] = 'Removed ' + decode
        disasm.comments[str(j + 3)] = 'Removed ' + decode3
        navigate_to(j, center=True, widget=hack_file_text_box)
        status_text.set('Found and removed endless loop at {} and {}'.format(
            '0x' + extend_zeroes(hexi(j << 2), 8),
            '0x' + extend_zeroes(hexi((j << 2) + 0xC), 8)
        ))
    else:
        simpledialog.messagebox._show('Sorry', 'The developer does not know how to bypass this CRC')
        return


manual_cic_win = None
def manual_cic():
    global manual_cic_win
    if not disassembler_loaded():
        return
    manual_cic_win = tk.Tk()
    texts = {
        str(0b00001): '6101',
        str(0b00010): '6102/7101',
        str(0b00100): '6103/7103',
        str(0b01000): '6105/7105',
        str(0b10000): '6106/7106'
    }


    def set_cic(num):
        for i in texts:
            if int(i) != num:
                buttons[i].deselect()
            else:
                buttons[i].select()
        if num == 1:
            num = 0b01000
        disasm.set_cic(CIC[texts[str(num)][:4]])
        app_config['CIC'][disasm.hack_file_name] = disasm.cic
        save_config()
        navigate_to(navigation)


    buttons = {}
    buttons[str(0b00001)] = tk.Checkbutton(manual_cic_win, text=texts[str(0b00001)], command=lambda: set_cic(0b00001))
    buttons[str(0b00010)] = tk.Checkbutton(manual_cic_win, text=texts[str(0b00010)], command=lambda: set_cic(0b00010))
    buttons[str(0b00100)] = tk.Checkbutton(manual_cic_win, text=texts[str(0b00100)], command=lambda: set_cic(0b00100))
    buttons[str(0b01000)] = tk.Checkbutton(manual_cic_win, text=texts[str(0b01000)], command=lambda: set_cic(0b01000))
    buttons[str(0b10000)] = tk.Checkbutton(manual_cic_win, text=texts[str(0b10000)], command=lambda: set_cic(0b10000))

    for i in CIC:
        if CIC[i] == disasm.cic:
            for j in buttons:
                if i == texts[j][:4]:
                    buttons[j].select()

    def manual_cic_win_equals_none():
        global manual_cic_win
        manual_cic_win.destroy()
        manual_cic_win = None

    [buttons[b].pack(anchor=tk.W) for b in buttons]

    manual_cic_win.protocol('WM_DELETE_WINDOW', manual_cic_win_equals_none)
    manual_cic_win.bind('<Escape>', lambda _: manual_cic_win_equals_none())
    manual_cic_win.bind('<FocusOut>', lambda _: manual_cic_win_equals_none())
    manual_cic_win.mainloop()


def toggle_calc_crc():
    if not disassembler_loaded():
        return
    app_config['calc_crc'][disasm.hack_file_name] = not app_config['calc_crc'][disasm.hack_file_name]
    save_config()


changes_win = changes_list_box = None
def scour_changes():
    global changes_win, changes_list_box
    if not disassembler_loaded():
        return
    ch_ch_changes = []  # don't wanna be a richer man
    percent = (disasm.file_length >> 2) // 100
    for i in range(0, disasm.file_length, 4):
        if not (i >> 2) & percent:
            status_text.set(str((i >> 2) // percent) + '%')
            window.update()
        for j in range(4):
            if disasm.hack_file[i+j] != disasm.base_file[i+j]:
                ch_ch_changes.append(i)
                break
    status_text.set('100%')
    display_list = []
    for i in ch_ch_changes:
        key = str(i >> 2)
        instruction = disasm.decode(int_of_4_byte_aligned_region(disasm.hack_file[i:i+4]), i >> 2)
        if not instruction:
            instruction = 'UNKNOWN/NOT AN INSTRUCTION'
        hex_of = hex_space(disasm.hack_file[i:i+4].hex().upper())
        if disasm.game_address_mode:
            i += disasm.game_offset
        address = extend_zeroes(hexi(i), 8)
        the_text = '{}: {}    {}'.format(address,hex_of,instruction)
        if key_in_dict(disasm.comments, key):
            the_text += '  | {}'.format(disasm.comments[key])
        display_list.append(the_text)

    def changes_win_equals_none():
        global changes_win
        changes_win.destroy()
        changes_win = None

    if changes_win:
        changes_win_equals_none()
    if display_list:
        changes_win = tk.Tk()
        changes_win.title('{} Differences'.format(len(display_list)))
        changes_win.geometry('900x500+50+50')
        changes_list_box = tk.Listbox(changes_win, font=('Courier', 10))
        [changes_list_box.insert(tk.END, i) for i in display_list]

        def list_callback():
            curselect = changes_list_box.curselection()
            if not curselect:
                return
            apply_hack_changes()
            apply_comment_changes()
            entry = changes_list_box.get(curselect[0])
            increment = 0 if not disasm.game_address_mode else -(disasm.game_offset >> 2)
            address = deci(entry[:8]) >> 2
            reset_target()
            navigate_to(address + increment, center=True, widget=None)

        changes_list_box.bind('<<ListboxSelect>>', lambda _: list_callback())
        changes_list_box.place(x=5, y=5, width=890, height=490)
        changes_win.protocol('WM_DELETE_WINDOW', changes_win_equals_none)
        changes_win.bind('<Escape>', lambda _: changes_win_equals_none())
        changes_win.bind('<F1>', lambda e: view_comments())
        changes_win.bind('<F3>', lambda e: toggle_base_file())
        changes_win.bind('<F4>', lambda e: navigation_prompt(root=changes_win))
        changes_win.bind('<F5>', lambda e: toggle_address_mode())
        changes_win.bind('<Control-s>', lambda e: save_changes_to_file())
        changes_win.bind('<Control-S>', lambda e: save_changes_to_file())
        changes_win.mainloop()


def tst():
    phrase = simpledialog.askstring('Find what','')
    if not phrase:
        return
    print('\n'*2)
    checking = disasm.hack_file[disasm.header_items['Boot Code'][0]:disasm.header_items['Boot Code'][1]]
    ints = ints_of_4_byte_aligned_region(checking)
    for j, i in enumerate(ints):
        j += 0x400
        decoded = disasm.decode(i, j)
        if phrase in decoded:
            print(hexi(j << 2) + ': ' + decoded)



menu_bar = tk.Menu(window)
auto_open = tk.BooleanVar()
calc_crc = tk.BooleanVar()
auto_open.set(app_config['open_roms_automatically'])

file_menu = tk.Menu(menu_bar, tearoff=0)
file_menu.add_command(label='Start new', command=lambda: open_files('new'))
file_menu.add_command(label='Open existing', command=lambda: open_files('existing'))
file_menu.add_separator()
file_menu.add_command(label='Save (Ctrl+S)', command=save_changes_to_file)
file_menu.add_command(label='Save as...', command=lambda: save_changes_to_file(True))
file_menu.add_separator()
file_menu.add_checkbutton(label='Auto open previous roms on startup', command=toggle_auto_open,
                          variable=auto_open)
file_menu.add_command(label='Exit', command=lambda: close_window('left'))
menu_bar.add_cascade(label='File', menu=file_menu)

nav_menu = tk.Menu(menu_bar, tearoff=0)
nav_menu.add_command(label='Browse Comments (F1)', command=view_comments)
nav_menu.add_command(label='Browse Jumps (F2)', command= lambda: find_jumps(just_window=True))
nav_menu.add_command(label='Navigate (F4)', command=navigation_prompt)
menu_bar.add_cascade(label='Navigation', menu=nav_menu)

tools_menu = tk.Menu(menu_bar, tearoff=0)
tools_menu.add_command(label='Set memory editor offset', command=set_mem_edit_offset)
tools_menu.add_command(label='Scour hack for differences', command=scour_changes)
tools_menu.add_command(label='Bypass CRC', command=bypass_crc)
tools_menu.add_command(label='Manually set CIC chip', command=manual_cic)
tools_menu.add_checkbutton(label='Calculate checksum when saving', command=toggle_calc_crc, variable=calc_crc)
# tools_menu.add_command(label='Test', command=tst)
menu_bar.add_cascade(label='Tools', menu=tools_menu)

opts_menu = tk.Menu(menu_bar, tearoff=0)
opts_menu.add_command(label='Toggle "game entry point" mode (F5)', command=toggle_address_mode)
opts_menu.add_command(label='Toggle hex mode (F6)', command=toggle_hex_mode)
opts_menu.add_command(label='Toggle hex space separation (F7)', command=toggle_hex_space)
opts_menu.add_command(label='Set immediate value identifier', command=change_immediate_id)
opts_menu.add_command(label='Set scroll amount', command=set_scroll_amount)
menu_bar.add_cascade(label='Options', menu=opts_menu)

win_menu = tk.Menu(menu_bar, tearoff=0)
win_menu.add_command(label='Change colour scheme', command=set_colour_scheme)
win_menu.add_command(label='Change window dimensions', command=change_win_dimensions)
win_menu.add_command(label='Toggle Base Textbox (F3)', command=toggle_base_file)
win_menu.add_command(label='Toggle Status Bar', command=toggle_status_bar)
menu_bar.add_cascade(label='Window', menu=win_menu)

help_menu = tk.Menu(menu_bar,tearoff=0)
help_menu.add_command(label='Usage Info', command=help_box)
help_menu.add_command(label='Opcodes list', command=opcodes_list)
help_menu.add_command(label='About', command=about_box)
menu_bar.add_cascade(label='Help', menu=help_menu)

window.config(menu=menu_bar)

window.bind('<F1>', lambda e: view_comments())
window.bind('<F2>', lambda e: find_jumps(just_window=True))
window.bind('<F3>', lambda e: toggle_base_file())
window.bind('<F4>', lambda e: navigation_prompt())
window.bind('<F5>', lambda e: toggle_address_mode())
window.bind('<F6>', lambda e: toggle_hex_mode())
window.bind('<F7>', lambda e: toggle_hex_space())
window.bind('<Control-s>', lambda e: save_changes_to_file())
window.bind('<Control-S>', lambda e: save_changes_to_file())
window.bind('<MouseWheel>', scroll_callback)
window.bind('<FocusOut>', lambda e: replace_clipboard())
hack_file_text_box.bind('<Control-g>', lambda e: find_jumps())
hack_file_text_box.bind('<Control-G>', lambda e: find_jumps())
hack_file_text_box.bind('<Control-f>', lambda e: follow_jump())
hack_file_text_box.bind('<Control-F>', lambda e: follow_jump())


def text_box_callback(event):
    global prev_cursor_location
    if disassembler_loaded():
        reset_target()
        apply_hack_changes()
        apply_comment_changes()
        def after_delay(event):
            global prev_cursor_location
            correct_cursor(event)
            line = get_cursor(event.widget)[1]
            if event.widget is hack_file_text_box:
                prev_cursor_location = (line - 1) + navigation
            highlight_stuff(widget=event.widget, skip_moving_cursor=True)
            if event.widget is address_text_box:
                window.clipboard_clear()
                cursor_start = '{}.{}'.format(line, 0)
                cursor_end = modify_cursor(cursor_start, 0, 'max', get_text_content(address_text_box))
                new_clip = address_text_box.get(cursor_start, cursor_end[0])
                window.clipboard_append(new_clip)
                status_text.set('Copied {} to clipboard'.format(new_clip))
        window.after(1, lambda: after_delay(event))

[tbox.bind('<Button-1>', text_box_callback) for tbox in ALL_TEXT_BOXES]

address_input = tk.Text(window, font=('Courier', 12))
address_output = tk.Text(window, font=('Courier', 12))
address_translate_button = tk.Button(window, text='Translate Address', command=lambda:translate_box(button=True),
                                     font=('Sans', 9))

address_input.bind('<Return>', lambda e: window.after(1, lambda: translate_box()))
address_input.bind('<Control-v>', lambda e: window.after(1, lambda: translate_box()))
address_input.bind('<Control-V>', lambda e: window.after(1, lambda: translate_box()))


address_input.place(x=6, y=8, width=88, height=21)
address_translate_button.place(x=98, y=8, height=21)
address_output.place(x=214, y=8, width=88, height=21)

auto_copy_var = tk.IntVar()
auto_copy_checkbtn = tk.Checkbutton(window, text='Auto-copy to clipboard', var=auto_copy_var, command=lambda:window.after(1,lambda:toggle_auto_copy()))
auto_copy_checkbtn.place(x=304, y=5)
auto_copy_var.set(app_config['auto_copy'])

def nav_button_callback():
    if not disassembler_loaded():
        return
    try:
        clip_content = window.clipboard_get()
        navigation_callback(clip_content)
    except:
        ''

nav_button = tk.Button(window, text='Navigate to address in clipboard', command=nav_button_callback,
                       font=('Sans', 9))
nav_button.place(x=500, y=8, height=21)


target_up_label = tk.Label(window)
target_down_label = tk.Label(window)
target_of_up_label = tk.Label(window)
target_of_down_label = tk.Label(window)

status_text = tk.StringVar()
status_text.set('Welcome!')
status_bar = tk.Label(window, relief=tk.SUNKEN, bd=3, textvariable=status_text, anchor=tk.W)

if app_config['status_bar']:
    status_bar.pack(side=tk.BOTTOM, fill=tk.X, padx=5, pady=3)

set_widget_sizes()
window.protocol('WM_DELETE_WINDOW', close_window)
change_colours()

if app_config['open_roms_automatically'] and app_config['previous_base_opened'] and app_config['previous_hack_opened']:
    if exists(app_config['previous_base_opened']) and exists(app_config['previous_hack_opened']):
        window.after(1, open_files)

window.mainloop()
