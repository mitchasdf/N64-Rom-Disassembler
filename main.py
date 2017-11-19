
import tkinter as tk
from tkinter import simpledialog, filedialog, colorchooser
import os
from function_defs import *
from disassembler import Disassembler, REGISTERS_ENCODE, BRANCH_INTS, JUMP_INTS
import webbrowser

CONFIG_FILE = 'rom disassembler.config'

BRANCH_FUNCTIONS = ['BEQ', 'BEQL', 'BGEZ', 'BGEZAL', 'BGEZALL', 'BGEZL', 'BGTZ', 'BGTZL', 'BLEZ', 'BLEZL',
                    'BLTZ', 'BLTZAL', 'BLTZALL', 'BLTZL', 'BNEZ', 'BNEL', 'BNE', 'BC1F', 'BC1FL', 'BC1T', 'BC1TL']

JUMP_FUNCTIONS = ['J', 'JR', 'JAL', 'JALR']

# Disassembler, created when opening files
disasm = None

window = tk.Tk()
window.title('ROM Disassembler')
window.geometry('1335x820+50+50')
window.iconbitmap('n64_disassembler.ico')

working_dir = os.path.dirname(os.path.realpath(__file__)) + '\\'
FRESH_APP_CONFIG = {
    'previous_base_location': working_dir,
    'previous_hack_location': working_dir,
    'scroll_amount': 8,
    'immediate_identifier': '$',
    'hex_mode': False,
    'hex_space_separation': True,
    'game_address_mode': False,
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

window.config(bg=app_config['window_background_colour'])
'''
    A GUI with custom behaviour is required for user-friendliness.
    
    Displaying the whole file at once in a text box causes lag, so these text boxes will
      need to hold a small amount of data at a time in order to run smoothly.
      
    This can be done by maintain max_lines amount of lines at all times.
    
    Deviating from max_lines causes the list containing the data for the syntax checker
      to have a "shift". The syntax checker can't assume where, or whether or not, a shift
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
[text_box.config(font=('Courier',12), state=tk.DISABLED,
                 bg=app_config['text_bg_colour'], fg=app_config['text_fg_colour'])
 for text_box in ALL_TEXT_BOXES]

[text_box.tag_config('cursor_line', background=app_config['cursor_line_colour']) for text_box in ALL_TEXT_BOXES]
[hack_file_text_box.tag_config(tag, background=app_config['tag_config'][tag]) for tag in app_config['tag_config']]

# [current_buffer_position, [(navigation, cursor_location, text_box_content
# , immediate_id, game_address_mode, hex_mode), ...]]
# |-----------for hack_buffer only----------|
hack_buffer = [-1, []]
comments_buffer = [-1, []]
buffer_max = 20000

# {'decimal_address': [error_code, text_attempted_to_encode], ...}
user_errors = {}

max_lines = 42
navigation = 0


def save_config():
    pickle_data(app_config, CONFIG_FILE)


def change_colours(text_bg, text_fg, win_bg, cursor_line_colour, new_tag_config):
    [hack_file_text_box.tag_delete(tag) for tag in app_config['tag_config']]
    [(text_box.tag_delete('cursor_line'),
      text_box.tag_config('cursor_line', background=cursor_line_colour))
     for text_box in ALL_TEXT_BOXES]
    [hack_file_text_box.tag_config(tag, background=new_tag_config[tag]) for tag in new_tag_config]
    [text_box.config(bg=text_bg, fg=text_fg) for text_box in ALL_TEXT_BOXES]
    address_translate_button.config(bg=text_bg, fg=text_fg, activebackground=text_bg,activeforeground=text_fg)
    window.config(bg=win_bg)
    [handle.config(bg=text_bg,fg=text_fg) for handle in [address_input, address_output]]
    if change_rom_name_button:
        change_rom_name_button.config(bg=text_bg, fg=text_fg, activebackground=text_bg, activeforeground=text_fg)
    highlight_stuff(skip_moving_cursor=True)


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
    if key in user_errors:
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
    global max_lines
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
def highlight_stuff(event=None, skip_moving_cursor=False):
    global prev_reg_target, prev_address_target, prev_cursor_location

    [hack_file_text_box.tag_remove(tag, '1.0', tk.END) for tag in app_config['tag_config']]
    [text_box.tag_remove('cursor_line', '1.0', tk.END) for text_box in ALL_TEXT_BOXES]

    # I gave up looking for the 1 case causing attribute error
    # Attribute error has slowly become a feature
    try:
        if not event:
            cursor, c_line, column = get_cursor(hack_file_text_box)
            hack_function = True
        else:
            cursor, c_line, column = get_cursor(event.widget)
            hack_function = True if event.widget is hack_file_text_box else False
    except AttributeError:
        cursor, c_line, column = get_cursor(hack_file_text_box)
        hack_function = True
    text = get_text_content(hack_file_text_box).split('\n')
    targeting = get_word_at(text, c_line, column)


    if not prev_cursor_location:
        prev_cursor_location = navigation + c_line - 1
    elif not skip_moving_cursor:
        new_cursor = False
        if prev_cursor_location < navigation:
            new_cursor = '1.0'
        elif prev_cursor_location > navigation + max_lines:
            new_cursor = cursor_value(max_lines, 0)
        if new_cursor:
            hack_file_text_box.mark_set(tk.INSERT, new_cursor)
            cursor, c_line, column = get_cursor(hack_file_text_box)

    jumps_from = {}

    [text_box.tag_add('cursor_line',
                      cursor_value(c_line, 0),
                      cursor_value(c_line + 1, 0))
     for text_box in ALL_TEXT_BOXES]

    if prev_address_target or not column:
        c_line = 0

    for i in range(len(text)):
        navi = navigation + i
        if not app_config['hex_mode']:
            line = i + 1
            line_text = text[i]
            this_word = line_text[:line_text.find(' ')]
            imm_id = text[i].find(app_config['immediate_identifier'])
            address = None

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
                if not c_line:
                    address = prev_address_target
                elif line == c_line:
                    address = hex_address
                try:
                    possibly_value_error = deci(hex_address) >> 2
                    jumps_from[str(navi)] = possibly_value_error
                except ValueError:
                    ''


            # Highlight jump functions
            elif this_word in JUMP_FUNCTIONS:
                hack_file_text_box.tag_add('jump',
                                           cursor_value(line, 0),
                                           cursor_value(line, len(text[i])))
                hex_address = line_text[imm_id + 1:]
                if not c_line:
                    address = prev_address_target
                elif line == c_line and this_word in ['J', 'JAL']:
                    address = hex_address
                try:
                    possibly_value_error = deci(hex_address) >> 2
                    jumps_from[str(navi)] = possibly_value_error
                except ValueError:
                    ''

            elif line_text == 'NOP':
                hack_file_text_box.tag_add('nop',
                                           cursor_value(line, 0),
                                           cursor_value(line, len(text[i])))

            # Highlight the target of jump or branch functions
            if address:
                if hack_function:
                    if c_line:
                        try:
                            # Raises error if user types non-numeric characters where an address/offset is
                            address = deci(address)
                        except:
                            address = -1
                        else:
                            if app_config['game_address_mode']:
                                address -= disasm.game_offset
                            address >>= 2
                            prev_address_target = address
                            jumps_from[str(navi)] = address
                    if address in range(navigation, navigation + max_lines):
                        place = address - navigation
                        hack_file_text_box.tag_add('target',
                                                   cursor_value(place + 1, 0),
                                                   cursor_value(place + 2, 0))

            # Highlight instructions in which are a target of any jump or branch
            # Because the disasm.jumps dict is so huge, a try/except works "exceptionally" faster than iterative methods
            try:
                _ = disasm.jumps_to[str(navi)]

                # We only want to hit this line of code if str(navi) in disasm.jumps_to
                hack_file_text_box.tag_add('jump_to',
                                           cursor_value(line, 0),
                                           cursor_value(line + 1, 0))
            except KeyError:
                ''

            try:
                _ = disasm.branches_to[str(navi)]

                # We only want to hit this line of code if str(navi) in disasm.jumps_to
                hack_file_text_box.tag_add('branch_to',
                                           cursor_value(line, 0),
                                           cursor_value(line + 1, 0))
            except KeyError:
                ''

        # Highlight errors
        key = str(navi)
        if key in user_errors:
            err_code = user_errors[key][0]
            hack_file_text_box.tag_add('bad' if err_code > -3 else 'out_of_range',
                                       cursor_value(i + 1, 0),
                                       cursor_value(i + 2, 0))

    if not app_config['hex_mode'] and hack_function:
        for i in range(len(text)):
            navi = navigation + i
            try:
                address = jumps_from[str(navi)]
            except:
                continue
            if address == prev_cursor_location:
                # Highlight instructions in which jump to the cursors location
                hack_file_text_box.tag_add('jump_from',
                                           cursor_value(i + 1, 0),
                                           cursor_value(i + 2, 0))

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
    if targeting in REGISTERS_ENCODE:
        prev_reg_target = targeting
        highlight_targets(targeting)
    elif prev_reg_target:
        highlight_targets(prev_reg_target)


def reset_target():
    global prev_reg_target, prev_address_target, prev_cursor_location
    prev_reg_target, prev_address_target = '', 0
    prev_cursor_location = 0


# The hacked text box syntax checker and change applier
def apply_hack_changes(ignore_slot = None):
    current_text = get_text_content(hack_file_text_box).upper()
    split_text = current_text.split('\n')
    for i in range(max_lines):
        navi = navigation + i
        if i == ignore_slot:
            continue
        is_hex_part = navi < 16
        string_key = '{}'.format(navi)
        if is_hex_part or app_config['hex_mode']:
            without_spaces = split_text[i].replace(' ', '')
            try:
                if len(without_spaces) != 8:
                    raise Exception()
                int_of = deci(without_spaces)
            except:
                user_errors[string_key] = (-1, split_text[i])
                continue
            disasm.split_and_store_bytes(int_of, navi)
            clear_error(string_key)

        elif not split_text[i]:
            disasm.split_and_store_bytes(0, navi)
            hack_file_text_box.insert(cursor_value(i + 1, 0), 'NOP')

        elif split_text[i] != 'UNKNOWN/NOT AN INSTRUCTION':
            encoded_int = disasm.encode(split_text[i], navi)
            if encoded_int >= 0:
                disasm.split_and_store_bytes(encoded_int, navi)
                clear_error(string_key)
            else:
                user_errors[string_key] = (encoded_int, split_text[i])


# Disassembler.comments accumulator
def apply_comment_changes():
    current_text = get_text_content(comments_text_box)
    split_text = current_text.split('\n')
    config = app_config['jumps_displaying'][disasm.hack_file_name].copy()
    increment = disasm.game_offset if app_config['game_address_mode'] else 0
    for i in range(max_lines):
        navi = navigation + i
        string_key = '{}'.format(navi)
        hex_navi = extend_zeroes(hexi((navi << 2) + increment), 8)
        if not split_text[i]:
            if is_in_comments(string_key):
                del disasm.comments[string_key]
            for key in config:
                if key[:8] == hex_navi:
                    del app_config['jumps_displaying'][disasm.hack_file_name][key]
                    new_key = key[:19]
                    app_config['jumps_displaying'][disasm.hack_file_name][new_key] = config[key]
                    save_config()
                    if jumps_window:
                        try:
                            index = function_list_box.get(0, tk.END).index(key)
                            function_list_box.delete(index)
                            function_list_box.insert(index, new_key)
                        except ValueError:
                            ''
                    break
                breaking = False
                for i, address in enumerate(config[key]):
                    if address[:8] == hex_navi:
                        dictin = app_config['jumps_displaying'][disasm.hack_file_name][key]
                        dictin[i] = address[:8]
                        save_config()
                        if jumps_window:
                            try:
                                index = jump_list_box.get(0, tk.END).index(address)
                                jump_list_box.delete(index)
                                jump_list_box.insert(index, dictin[i])
                            except ValueError:
                                ''
                        breaking = True
                        break
                if breaking:
                    break
            continue
        if not is_in_comments(string_key):
            disasm.comments[string_key] = split_text[i]
        if split_text[i] == disasm.comments[string_key]:
            continue
        disasm.comments[string_key] = split_text[i]
        for key in config:
            if key[:8] == hex_navi:
                del app_config['jumps_displaying'][disasm.hack_file_name][key]
                new_key = key[:19] + ' ' + disasm.comments[string_key]
                app_config['jumps_displaying'][disasm.hack_file_name][new_key] = config[key]
                save_config()
                if jumps_window:
                    try:
                        index = function_list_box.get(0, tk.END).index(key)
                        function_list_box.delete(index)
                        function_list_box.insert(index, new_key)
                    except ValueError:
                        ''
                break
            breaking = False
            for i, address in enumerate(config[key]):
                if address[:8] == hex_navi:
                    dictin = app_config['jumps_displaying'][disasm.hack_file_name][key]
                    dictin[i] = address[:8] + ' ' + disasm.comments[string_key]
                    save_config()
                    if jumps_window:
                        try:
                            index = jump_list_box.get(0, tk.END).index(address)
                            jump_list_box.delete(index)
                            jump_list_box.insert(index, dictin[i])
                        except ValueError:
                            ''
                    breaking = True
                    break
            if breaking:
                break



def buffer_append(buffer, tuple):
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
    if not disasm:
        return
    reset_target()
    joined_text = get_text_content(handle)
    split_text = joined_text.split('\n')

    cursor, line, column = get_cursor(handle)

    ctrl_held = bool(event.state & 0x0004)
    ctrl_d = ctrl_held and event.keysym == 'd'
    bad_bad_hotkey = ctrl_held and event.keysym in ['z', 't']
    if bad_bad_hotkey:
        # Messes with custom behaviour, so wait until after changes are made
        #   by bad hotkey, then restore text box to how it was before the hotkey
        window.after(0, lambda: (handle.delete('1.0', tk.END),
                                 handle.insert('1.0', joined_text),
                                 handle.mark_set(tk.INSERT, cursor),
                                 highlight_stuff()))
        return

    shift_held = bool(event.state & 0x0001)
    alt_held = bool(event.state & 0x0008) or bool(event.state & 0x0080)
    has_char = bool(event.char) and event.keysym != 'Escape' and not ctrl_held
    just_function_key = event.keysym in ['Alt_L', 'Alt_R', 'Shift_L', 'Shift_R', 'Control_L', 'Control_R']

    is_undoing = buffer and ctrl_held and event.keysym == 'comma'
    is_redoing = buffer and ctrl_held and event.keysym == 'period'
    is_cutting = ctrl_held and event.keysym == 'x'
    is_pasting = ctrl_held and event.keysym == 'v'
    is_copying = ctrl_held and event.keysym == 'c'
    is_deleting = ctrl_d or event.keysym == 'Delete'
    is_backspacing = event.keysym == 'BackSpace'
    is_returning = event.keysym == 'Return'

    selection_removable = has_char or is_pasting or is_cutting or is_deleting

    not_arrows = event.keysym not in ['Left', 'Up', 'Right', 'Down']
    vert_arrows = event.keysym in ['Up', 'Down']

    apply_function = apply_hack_changes if hack_function else lambda ignore_slot=None: apply_comment_changes()

    # Cause each modification of text box to snap-shot data in order to undo/redo
    if buffer and ((not (is_undoing or is_redoing) and has_char and not_arrows) or ctrl_d or is_pasting or is_cutting):
        buffer_frame = (navigation, cursor, joined_text,
                        app_config['immediate_identifier'],
                        app_config['game_address_mode'],
                        app_config['hex_mode'])\
                        if hack_function else \
                        (navigation, cursor, joined_text)
        buffer_append(buffer, buffer_frame)

    # Undoing and Redoing code
    if is_undoing or is_redoing:
        if buffer[0] == len(buffer[1]) - 1 and is_undoing:
            part = buffer[1][buffer[0]]
            if part[0] != navigation or part[2] != joined_text:
                buffer_frame = (navigation, cursor, joined_text,
                                app_config['immediate_identifier'],
                                app_config['game_address_mode'],
                                app_config['hex_mode']) \
                                if hack_function else \
                                (navigation, cursor, joined_text)
                buffer_append(buffer, buffer_frame)

        buffer[0] += 1 if is_redoing else -1
        if buffer[0] < 0:
            buffer[0] = 0
        elif buffer[0] > len(buffer[1]) - 1:
            buffer[0] = len(buffer[1]) - 1
        else:
            apply_hack_changes()
            apply_comment_changes()
            place = buffer[0]
            navigate_to(buffer[1][place][0])
            cursor = buffer[1][place][1]
            text_content = buffer[1][place][2]

            handle.delete('1.0', tk.END)
            handle.insert('1.0', text_content)
            handle.mark_set(tk.INSERT, cursor)

            if hack_function:
                immediate_id = buffer[1][place][3]
                game_address_mode = buffer[1][place][4]
                hex_mode = buffer[1][place][5]
                if immediate_id != app_config['immediate_identifier']:
                    app_config['immediate_identifier'] = immediate_id
                    save_config()
                    disasm.immediate_identifier = immediate_id
                if game_address_mode != app_config['game_address_mode']:
                    app_config['game_address_mode'] = game_address_mode
                    save_config()
                if hex_mode != app_config['hex_mode']:
                    app_config['hex_mode'] = hex_mode
                    save_config()
                    disasm.game_address_mode = game_address_mode
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
            #     selection_line_mod += 1
            #     selection_start, sel_start_line, sel_start_column = modify_cursor(selection_start, 1, 0, split_text)
            if sel_end_column == 0:
                selection_line_mod = True
                selection_end, sel_end_line, sel_end_column = modify_cursor(selection_end, -1, 0, split_text)
            selection_start, sel_start_line, sel_start_column = modify_cursor(selection_start, 0, 'min', split_text)
            selection_end, sel_end_line, sel_end_column = modify_cursor(selection_end, 0, 'max', split_text)
    except:
        selection_start, sel_start_line, sel_start_column = '1.0', 0, 0
        selection_end, sel_end_line, sel_end_column = '1.0', 0, 0

    has_selection = selection_start != selection_end
    selection_lines = sel_end_line - sel_start_line
    selection_function = has_selection and (selection_removable or is_copying)
    standard_key = not is_backspacing and not is_returning and has_char
    temp_cursor, _, __ = modify_cursor(selection_start, 0, -1, split_text)
    lower_outer_bound_selection_char = handle.get(temp_cursor)
    upper_outer_bound_selection_char = handle.get(selection_end)
    paste_text = ''
    lines_diff = 0

    # Because using mark_set() on SEL_FIRST or SEL_LAST seems to corrupt the widgets beyond repair at a windows level,
    # A work around with a custom clipboard is required in order for the code to be able to serve it's intended purpose
    if has_selection and not selection_lines:
        if is_pasting and '\n' in clipboard:
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
            move_amount = 1 if is_pasting else 0
            temp_cursor, _, __ = modify_cursor(handle.index(tk.INSERT), move_amount, 'max', get_text_content(handle))
            handle.mark_set(tk.INSERT, temp_cursor)
        handle.insert(insertion_place, paste_text)
        if not selection_line_mod or is_pasting:
            window.after(0, lambda: (apply_hack_changes(),
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

    nl_at_cursor = handle.get(cursor) == '\n'
    if has_selection:
        nl_at_cursor = nl_at_cursor or handle.get(selection_end) == '\n'
    # Make any key delete the final character of the line if word is about to wrap onto next line
    # Also validate all code except line currently editing
    if standard_key:
        apply_function(ignore_slot = line - 1)
        line_chars = len(split_text[line - 1])
        if line_chars > max_char - 1:
            handle.delete(cursor_value(line, max_char - 1), cursor_value(line, max_char))

    # Make delete do nothing if cursor precedes a new line
    # Make backspace act as left arrow if cursor at column 0 then validate code (ignoring the line if cursor not at column 0)
    elif ((is_backspacing and (column == 0 and line > 1)) or (is_deleting and nl_at_cursor and not shift_held)) and not has_selection:
        # if not selection_lines: # was needed to stop something but now is not?
        if is_deleting:
            apply_function(ignore_slot = (line - 1) if not sel_start_line else None)
        handle.insert(cursor,'\n')
        handle.mark_set(tk.INSERT, cursor)
        if is_backspacing:
            apply_function(ignore_slot = (line - 1) if not sel_start_line else None)

    # Make return send the cursor to the end of the next line and validate code
    elif is_returning:
        move_lines = -1 if line == max_lines else 0
        cursor, _, __ = modify_cursor(cursor, move_lines, 'max', split_text)
        handle.mark_set(tk.INSERT, cursor)
        handle.delete(cursor)
        new_cursor, _, __ = modify_cursor(cursor, 1, 'max', split_text)
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
            window.after(0, lambda: (apply_comment_changes(), highlight_stuff(event)))
        else:
            window.after(0, lambda: highlight_stuff(event))


base_file_text_box.bind('<Key>', lambda event:
    keyboard_events(base_file_text_box, 31, event, buffer=False))

hack_file_text_box.bind('<Key>', lambda event:
    keyboard_events(hack_file_text_box, 31, event, buffer=hack_buffer, hack_function=True))

comments_text_box.bind('<Key>', lambda event:
    keyboard_events(comments_text_box, 59, event, buffer=comments_buffer))


# The button is destroyed and remade every time the user scrolls within it's view
change_rom_name_button = tk.Button()
def change_rom_name():
    if not disasm:
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


def navigate_to(index):
    global navigation, change_rom_name_button, disasm
    if not disasm:
        return

    destroy_change_rom_name_button()

    shift_amount = navigation

    # Correct the navigation if traveling out of bounds, also calculate limits for file samples to display
    amount_words = disasm.file_length // 4
    navigation = index if index + max_lines < amount_words else amount_words - max_lines
    if navigation < 0:
        navigation = 0
    limit = navigation + max_lines if navigation + max_lines < amount_words else amount_words
    lines = limit - navigation

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
        y_offset = ((disasm.header_items['Rom Name'][0] // 4) - navigation) * 18
        change_rom_name_button.place(x = 965, y = 36 + y_offset, height = 21)

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

    if prev_cursor_location in range(navigation, limit):
        line = prev_cursor_location - navigation
        temp_cursor, _, __ = modify_cursor('1.0', line, 'max', get_text_content(hack_file_text_box))
        hack_file_text_box.mark_set(tk.INSERT, temp_cursor)

    highlight_stuff()


def navigation_prompt():
    if not disasm:
        return
    address = simpledialog.askstring('Navigate to address', '')
    try:
        address = deci(address)
        if app_config['game_address_mode']:
            address -= disasm.game_offset
        address //= 4
    except:
        return
    apply_hack_changes()
    apply_comment_changes()
    reset_target()
    navigate_to(address)


def scroll_callback(event):
    global navigation, disasm
    if not disasm:
        return
    apply_hack_changes()
    apply_comment_changes()
    direction = -app_config['scroll_amount'] if event.delta > 0 else app_config['scroll_amount']
    navigate_to(navigation + direction)


def save_changes_to_file(save_as=False):
    global max_lines, disasm
    if not disasm:
        return False

    apply_hack_changes()
    apply_comment_changes()

    # Do not save changes if there are errors
    for key in user_errors:
        i = int(key)
        navigate_to(i - (max_lines // 2))
        highlight_stuff()
        return False

    if save_as:
        new_file_name = filedialog.asksaveasfilename(initialdir = app_config['previous_hack_location'],
                                                     title = 'Save as...')
        if not new_file_name:
            return False
        new_file_path = os.path.realpath(new_file_name)
        if new_file_path == disasm.base_folder + disasm.base_file_name:
            simpledialog.messagebox._show('Wait a sec', 'You shouldn\'t select the base file')
            return False
        new_dir = new_file_path[:new_file_path.rfind('\\') + 1]
        app_config['previous_hack_location'] = new_dir
        save_config()
        disasm.hack_file_name = new_file_name
        disasm.comments_file = new_file_name + '.comments'

    with open(disasm.comments_file + '(Backup).txt', 'w') as backup_comments_file:
        with open(disasm.comments_file, 'r') as comments_file:
            backup_comments_file.write(comments_file.read())

    try:
        with open(disasm.comments_file, 'w') as file:
            file.write(dict_to_string(disasm.comments))
        os.remove(disasm.comments_file + '(Backup).txt')
    except Exception as e:
        simpledialog.messagebox._show('Error', 'There was trouble saving your comments file. '
                                               'A backup of your old comments can be found next to the original comments file.'
                                               '\nYou should go there now and rescue the backup before attempting to save again.'
                                               '\nIf you save again, that backup will be over-written.'
                                               '\n\n' + str(e))

    with open(disasm.hack_folder + disasm.hack_file_name, 'wb') as file:
        file.write(disasm.hack_file)

    return True


def close_window(side = 'right'):
    def destroy_them():
        if colours_window:
            colours_window.destroy()
        if jumps_window:
            jumps_window.destroy()
        if comments_window:
            comments_window.destroy()
        window.destroy()

    if not disasm:
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
    global disasm

    if disasm:
        if not save_changes_to_file():
            return
        disasm = None
        [text_box.delete('1.0', tk.END) for text_box in ALL_TEXT_BOXES]
    else:
        [text_box.configure(state=tk.NORMAL) for text_box in ALL_TEXT_BOXES]
    destroy_change_rom_name_button()

    # Set data for rest of this function
    if mode == 'new':
        base_title = 'Select the original base rom'
        hack_title = 'Choose location and name for the new hacked rom'
        hack_dialog_function = filedialog.asksaveasfilename
    else:
        base_title = 'Select the base rom'
        hack_title = 'Select the hacked rom'
        hack_dialog_function = filedialog.askopenfilename

    # Obtain file locations from user input
    if open_my_roms_automatically:
        base_file_path = 'C:\\Users\\Mitchell\\Desktop\\n64split\\base sm64.z64'
    else:
        base_file_path = filedialog.askopenfilename(initialdir = app_config['previous_base_location'], title = base_title)
    if not base_file_path:
        return
    base_file_path = os.path.realpath(base_file_path)
    base_dir = base_file_path[:base_file_path.rfind('\\') + 1]

    hack_dir = base_dir if mode == 'new' else app_config['previous_hack_location']
    if open_my_roms_automatically:
        hack_file_path = 'C:\\Users\\Mitchell\\Desktop\\n64split\\hack sm64.z64'
    else:
        hack_file_path = hack_dialog_function(initialdir = hack_dir, title = hack_title)
    if not hack_file_path:
        return
    base_dot = base_file_path.rfind('.')
    file_extension = base_file_path[base_dot + 1:]
    if '.' not in hack_file_path:
        hack_file_path += '.' + file_extension
    else:
        hack_dot = hack_file_path.rfind('.')
        if hack_dot == len(hack_file_path) - 1:
            hack_file_path += file_extension
    hack_file_path = os.path.realpath(hack_file_path)
    hack_dir = hack_file_path[:hack_file_path.rfind('\\') + 1]

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
                              app_config['game_address_mode'],
                              app_config['immediate_identifier'])
    except Exception as e:
        simpledialog.messagebox._show('Error', e)
        base_file_text_box.delete('1.0', tk.END)
        hack_file_text_box.delete('1.0', tk.END)
        [text_box.configure(state=tk.DISABLED) for text_box in ALL_TEXT_BOXES]
        disasm = None
        return

    base_file_text_box.insert('1.0', 'Mapping out jumps...')
    hack_file_text_box.insert('1.0', 'Please wait')

    def rest_of_function():
        disasm.map_jumps()
        base_file_text_box.delete('1.0', tk.END)
        hack_file_text_box.delete('1.0', tk.END)

        # Navigate user to first line of code, start the undo buffer with the current data on screen
        navigate_to(0)
        buffer_append(hack_buffer, (navigation,
                                    cursor_value(1, 0),
                                    get_text_content(hack_file_text_box),
                                    app_config['immediate_identifier'],
                                    app_config['game_address_mode'],
                                    app_config['hex_mode']))
        buffer_append(comments_buffer, (navigation,
                                        cursor_value(1, 0),
                                        get_text_content(comments_text_box)))
        if disasm.hack_file_name not in app_config['jumps_displaying']:
            app_config['jumps_displaying'][disasm.hack_file_name] = {}
        timer_tick('Disasm init')

        # ints = ints_of_4_byte_aligned_region(disasm.hack_file)
        # timer_tick('Creating ints list')
        # for i in range(len(ints)):
        #     instruction = disasm.decode(ints[i], i)
        # timer_tick('Disassembling file')

    # Otherwise text boxes don't get updated to notify user of task
    window.after(1, rest_of_function)


def toggle_address_mode():
    global function_list_box, jump_list_box
    apply_hack_changes()
    apply_comment_changes()
    cursor, line, column = get_cursor(hack_file_text_box)
    buffer_append(hack_buffer, (navigation,
                                hack_file_text_box.index(tk.INSERT),
                                get_text_content(hack_file_text_box),
                                app_config['immediate_identifier'],
                                app_config['game_address_mode'],
                                app_config['hex_mode']))
    toggle_to = not app_config['game_address_mode']
    app_config['game_address_mode'] = toggle_to
    if disasm:
        disasm.game_address_mode = toggle_to
    save_config()
    navigate_to(navigation)
    hack_file_text_box.mark_set(tk.INSERT, cursor)
    highlight_stuff(skip_moving_cursor=True)
    increment = disasm.game_offset if toggle_to else -disasm.game_offset
    config_data = app_config['jumps_displaying'][disasm.hack_file_name].copy()
    for key in config_data:
        del app_config['jumps_displaying'][disasm.hack_file_name][key]
        addy_1 = deci(key[:8]) + increment
        addy_2 = deci(key[11:19]) + increment
        comment = key[19:]
        addy_1 = extend_zeroes(hexi(addy_1), 8)
        addy_2 = extend_zeroes(hexi(addy_2), 8)
        new_key = '{} - {}{}'.format(addy_1, addy_2, comment)
        app_config['jumps_displaying'][disasm.hack_file_name][new_key] = []
        for address in config_data[key]:
            new_address = extend_zeroes(hexi(deci(address) + increment), 8)
            app_config['jumps_displaying'][disasm.hack_file_name][new_key].append(new_address)
    save_config()
    if jumps_window:
        function_list_box.delete(0, tk.END)
        jump_list_box.delete(0, tk.END)
        for key in app_config['jumps_displaying'][disasm.hack_file_name]:
            function_list_box.insert(tk.END, key)


def toggle_hex_mode():
    apply_hack_changes()
    apply_comment_changes()
    cursor, line, column = get_cursor(hack_file_text_box)
    buffer_append(hack_buffer, (navigation,
                                cursor,
                                get_text_content(hack_file_text_box),
                                app_config['immediate_identifier'],
                                app_config['game_address_mode'],
                                app_config['hex_mode']))
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
    accepted_symbols = ['<', '>', ':', ';', '\'', '"', '|', '{', '}',
                        '=', '+', '-', '_', '*', '&', '^', '%', '$', '#',
                        '@', '!', '`', '~', '/', '?', '\\']
    symbol = simpledialog.askstring('Set immediate identifier symbol',
                                    'Must be one of {}'.format(' '.join(accepted_symbols)))
    if symbol and symbol[:1] in accepted_symbols:
        hack_text = get_text_content(hack_file_text_box)
        buffer_append(hack_buffer, (navigation,
                                    hack_file_text_box.index(tk.INSERT),
                                    hack_text,
                                    app_config['immediate_identifier'],
                                    app_config['game_address_mode'],
                                    app_config['hex_mode']))
        hack_text.replace(app_config['immediate_identifier'], symbol[:1])
        hack_file_text_box.delete('1.0', tk.END)
        hack_file_text_box.insert('1.0', hack_text)
        for key in user_errors.keys():
            user_errors[key] = user_errors[key].replace(app_config['immediate_identifier'], symbol[:1])
        app_config['immediate_identifier'] = symbol[:1]
        if disasm:
            disasm.immediate_identifier = symbol[:1]
        save_config()


def set_scroll_amount():
    amount = simpledialog.askstring('Set scroll amount', 'Current: {}'.format(app_config['scroll_amount']))
    try:
        amount = deci(amount) if amount[:2] == '0x' else int(amount)
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
        'Once you have decided the name for your hacked rom, it is best not to ever change it. '
        'If you do change it, you will lose your app data pertaining to that rom.',
        '',
        'In order to save any changes you have made, all errors must be corrected before the save feature will allow it. '
        'Trying to save while an error exists will result in your navigation shifting to the next error instead.',
        '',
        'The comments file will be output to where your hacked rom is located. '
        'The comments file must always be located in the same folder as your hacked rom in order for it to load. '
        'You can also open the comments files with a text editor if required.',
        '',
        'When setting the scroll amount, use "0x" to specify a hexadecimal value, or leave it out to specify a decimal value.',
        '',
        'The "Translate Address" section is for addresses you find with your memory editor to be translated to the corresponding '
        'memory address for your emulator. In order to use this feature you will have to grab the game entry point address from '
        'your memory editor and paste it into "Options->Set memory editor offset". After this you can paste your addresses you '
        'need translating into the left text box.'
    ])
    message_2 = '\n'.join([
        '----Highlighting----',
        'There are different coloured highlights for jumps/branches and more. See "Options->Change Colour Scheme" '
        'for more details',
        '',
        '',
        '----Keyboard----',
        'Ctrl+S: Quick save',
        'Ctrl+F: Follow jump/branch at text insert cursor',
        'Ctrl+G: Find all jumps to function at text insert cursor',
        'F4: Navigate to address',
        'F5: Toggle mode which displays and handles addresses using the game\'s entry point',
        'F6: Toggle hex mode',
        'F7: Toggle byte separation during hex mode',
        'Ctrl+{Comma} ("<" key): Undo',
        'Ctrl+{Fullstop} (">" key): Redo',
        '',
        'The hacked rom text box and comments text box have separate undo/redo buffers. '
        'Both buffers can hold up to 20,000  frames each.',
        '',
        '',
        '----Jumps window----',
        'Click on any instruction inside the function (within the hack text box) that you wish to find all jumps to. '
        'This will open your jumps window and from there you can use it to navigate around all jumps to the function. '
        'If no functions show up when you press Ctrl+G, then there aren\'t any jumps to that function. Be on the lookout '
        'for the highlighted instructions for "Targets of any Jump" in your colour scheme menu. '
        'The functions in the jump window will be memorised until you delete them. '
        'You can delete functions from memory by selecting it in the jumps window and '
        'pressing the delete key. '
        'Adding a comment to the very top of the function using the comments text box will cause it to become labeled '
        'as such within the jumps window.'
    ])
    simpledialog.messagebox._show('Help', message)
    simpledialog.messagebox._show('Help (continued)', message_2)


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
# jumps_displaying = {}
def find_jumps(just_window=False):
    global jumps_window, function_list_box, jump_list_box
    if not disasm:
        return
    if just_window:
        cursor, line, column = '1.0', 1, 0
    else:
        cursor, line, column = get_cursor(hack_file_text_box)
    navi = (line - 1) + navigation
    jumps, function_start, function_end = disasm.find_jumps(navi)
    jumps_displaying = app_config['jumps_displaying'][disasm.hack_file_name]

    if not jumps_window:
        jumps_window = tk.Tk()
        jumps_window.title('Jumps to Functions')
        jumps_window.geometry('{}x{}'.format(600,460))
        jumps_window.bind('<F5>', lambda e: toggle_address_mode())
        function_list_box = tk.Listbox(jumps_window, font=('Courier', 12))
        jump_list_box = tk.Listbox(jumps_window, font=('Courier', 12))

        def function_list_callback():
            curselect = function_list_box.curselection()
            if not curselect:
                return
            key = function_list_box.get(curselect[0])
            increment = 0 if not app_config['game_address_mode'] else -(disasm.game_offset >> 2)
            start_address = deci(key[:8]) >> 2
            half_way = max_lines >> 1
            navigate_to((start_address - half_way) + increment)
            hack_file_text_box.mark_set(tk.INSERT,
                                        modify_cursor('1.0', half_way, 'max', get_text_content(hack_file_text_box))[0])
            highlight_stuff(skip_moving_cursor=True)

            jump_list_box.delete(0, tk.END)
            for address in jumps_displaying[key]:
                jump_list_box.insert(tk.END, address)

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
            increment = 0 if not app_config['game_address_mode'] else -(disasm.game_offset >> 2)
            address = jump_list_box.get(curselect[0])[:8]
            navi = (deci(address) >> 2) + increment
            navigate_to(navi - (max_lines >> 1))
            hack_file_text_box.mark_set(tk.INSERT,
                                        modify_cursor('1.0', max_lines >> 1, 'max', get_text_content(hack_file_text_box))[0])
            highlight_stuff(skip_moving_cursor=True)

        function_list_box.bind('<<ListboxSelect>>', lambda _: function_list_callback())
        jump_list_box.bind('<<ListboxSelect>>', lambda _: jump_list_callback())
        function_list_box.bind('<Key>', function_list_key)
        for key in jumps_displaying:
            function_list_box.insert(tk.END,key)
        tk.Label(jumps_window, text='Functions').place(x=6,y=5)
        tk.Label(jumps_window, text='Jumps to Function').place(x=6,y=232)
        function_list_box.place(x=5,y=27,width=590,height=200)
        jump_list_box.place(x=5,y=255,width=590,height=200)
        def jumps_window_equals_none():
            global jumps_window
            jumps_window.destroy()
            jumps_window = None
        jumps_window.protocol('WM_DELETE_WINDOW', jumps_window_equals_none)
        jumps_window.after(1, lambda: jumps_window.mainloop())
    elif jumps:
        jumps_window.focus_force()
    key = extend_zeroes(hexi(function_start << 2),8) + ' - ' + extend_zeroes(hexi(function_end << 2),8)
    key_not_in_jumps_displaying = True
    config = jumps_displaying.copy()
    increment = -disasm.game_offset if app_config['game_address_mode'] else 0
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
def view_comments():
    global comments_window
    if not disasm:
        return
    if comments_window:
        comments_window.focus_force()
    else:
        comments_window = tk.Tk()
        comments_window.title('Comments')
        comments_window.geometry('{}x{}'.format(710,510))
        comments_list = tk.Listbox(comments_window, font=('Courier', 12))
        comments_list.place(x=5,y=5,width=700,height=500)
        for key in sorted([int(key) for key in disasm.comments]):
            comments_list.insert(tk.END, '{}: {}'.format(extend_zeroes(hexi(key << 2),8), disasm.comments[str(key)]))
        def comments_list_callback(event):
            curselect = comments_list.curselection()
            if not curselect:
                return
            navi = deci(comments_list.get(curselect[0])[:8]) >> 2
            navigate_to(navi - (max_lines >> 1))
            new_cursor, line, column = modify_cursor('1.0', max_lines >> 1, 'max', get_text_content(comments_text_box))
            hack_file_text_box.mark_set(tk.INSERT, '{}.0'.format(line))
            # comments_text_box.focus_force()
            comments_text_box.mark_set(tk.INSERT, new_cursor)
            highlight_stuff(skip_moving_cursor=True)
        comments_list.bind('<<ListboxSelect>>', comments_list_callback)
        def comments_window_equals_none():
            global comments_window
            comments_window.destroy()
            comments_window = None
        comments_window.protocol('WM_DELETE_WINDOW', comments_window_equals_none)
        comments_window.mainloop()


def follow_jump():
    if not disasm:
        return
    cursor, line, column = get_cursor(hack_file_text_box)
    navi = (line - 1) + navigation
    navi_4 = navi << 2
    int_word = ints_of_4_byte_aligned_region(disasm.hack_file[navi_4: navi_4 + 4])[0]
    opcode = (int_word & 0xFC000000) >> 26
    navi += 1  # Address calculated based on address of delay slot
    address = 0
    if JUMP_INTS[opcode]:
        address = (int_word & 0x03FFFFFF) + (navi & 0x3C000000)
    elif BRANCH_INTS[opcode]:
        address = sign_16_bit_value(int_word & 0xFFFF) + navi
    if address:
        text_box_contents = get_text_content(hack_file_text_box)
        # (navigation, cursor_location, text_box_content, immediate_id, game_address_mode)
        frame = hack_buffer[1][hack_buffer[0]]
        if frame[0] != navigation or frame[1] != cursor or frame[2] != text_box_contents:
            buffer_append(hack_buffer,
                        (navigation, cursor, text_box_contents, app_config['immediate_identifier'],
                         app_config['game_address_mode'], app_config['hex_mode']))
        half_way = max_lines >> 1
        navigate_to(address - half_way)
        text_box_contents = get_text_content(hack_file_text_box)
        cursor_loc = modify_cursor(cursor_value(half_way, 0), 1, 'max', text_box_contents)[0]
        hack_file_text_box.mark_set(tk.INSERT, cursor_loc)
        buffer_append(hack_buffer,
                    (navigation, cursor_loc, text_box_contents,
                    app_config['immediate_identifier'], app_config['game_address_mode'], app_config['hex_mode']))
        highlight_stuff(skip_moving_cursor=True)



colours_window = None

def set_colour_scheme():
    global colours_window
    if colours_window:
        return
    if not disasm:
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
            int_colour = int('0x' + with_colour[1:], 16)
            r, g, b = (int_colour & 0xFF0000) >> 16, (int_colour & 0xFF00) >> 8, int_colour & 0xFF
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
        change_colours(app_config['text_bg_colour'], app_config['text_fg_colour'],
                       app_config['window_background_colour'], app_config['cursor_line_colour'],
                       app_config['tag_config'])
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
    colours_window.protocol('WM_DELETE_WINDOW', close_colours_win)
    colours_window.mainloop()


def set_mem_edit_offset():
    if not disasm:
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


def translate_box():
    if not disasm:
        return
    address_output.delete('1.0', tk.END)
    address_to_translate = address_input.get('1.0',tk.END).replace('\n','')
    try:
        if disasm.hack_file_name not in app_config['mem_edit_offset']:
            simpledialog.messagebox._show('Warning','You have not set your memory editor offset yet.')
            raise Exception()
        int_address = deci(address_to_translate)
        mem_offset = app_config['mem_edit_offset'][disasm.hack_file_name]
        address_output.insert('1.0', extend_zeroes(hexi((int_address - mem_offset) + disasm.game_offset), 8))
    except:
        address_output.insert('1.0', 'Error')
    finally:
        address_input.delete('1.0', tk.END)


def test_function():
    pass


def testthing():
    cursor, line, column = get_cursor(hack_file_text_box)
    nav = (line + navigation - 1)
    navi = nav << 2
    inter = int_of_4_byte_aligned_region(disasm.hack_file[navi:navi+4])
    disasm.comments[nav] = extend_zeroes(bin(inter)[2:], 32)


menu_bar = tk.Menu(window)

MENU_BACKGROUND = '#FFFFFF'
MENU_FOREGROUND = '#000000'

file_menu = tk.Menu(menu_bar, tearoff=0, bg=MENU_BACKGROUND, fg=MENU_FOREGROUND)
file_menu.add_command(label='Start new', command=lambda: open_files('new'))
file_menu.add_command(label='Open existing', command=lambda: open_files('existing'))
file_menu.add_separator()
file_menu.add_command(label='Save (Ctrl+S)', command=save_changes_to_file)
file_menu.add_command(label='Save as...', command=lambda: save_changes_to_file(True))
file_menu.add_separator()
file_menu.add_command(label='Exit', command=lambda: close_window('left'))
menu_bar.add_cascade(label='File', menu=file_menu)

nav_menu = tk.Menu(menu_bar, tearoff=0, bg=MENU_BACKGROUND, fg=MENU_FOREGROUND)
nav_menu.add_command(label='Navigate (F4)', command=navigation_prompt)
nav_menu.add_command(label='Browse Comments', command=view_comments)
nav_menu.add_command(label='Browse Jumps', command= lambda: find_jumps(just_window=True))
# ----------------------------------------------------------------------------------
# nav_menu.add_separator()
# nav_menu.add_command(label='Test', command=test_function)
# ----------------------------------------------------------------------------------

menu_bar.add_cascade(label='Navigation', menu=nav_menu)

opts_menu = tk.Menu(menu_bar, tearoff=0, bg=MENU_BACKGROUND, fg=MENU_FOREGROUND)
opts_menu.add_command(label='Change colour scheme', command=set_colour_scheme)
opts_menu.add_command(label='Toggle "game entry point" mode (F5)', command=toggle_address_mode)
opts_menu.add_command(label='Toggle hex mode (F6)', command=toggle_hex_mode)
opts_menu.add_command(label='Toggle hex space separation (F7)', command=toggle_hex_space)
opts_menu.add_command(label='Change immediate value identifier', command=change_immediate_id)
opts_menu.add_command(label='Set memory editor offset', command=set_mem_edit_offset)
opts_menu.add_command(label='Set scroll amount', command=set_scroll_amount)
menu_bar.add_cascade(label='Options', menu=opts_menu)

help_menu = tk.Menu(menu_bar,tearoff=0, bg=MENU_BACKGROUND, fg=MENU_FOREGROUND)
help_menu.add_command(label='Help', command=help_box)
help_menu.add_command(label='About', command=about_box)
menu_bar.add_cascade(label='Help', menu=help_menu)

window.config(menu=menu_bar)

window.bind('<F4>', lambda e: navigation_prompt())
window.bind('<F5>', lambda e: toggle_address_mode())
window.bind('<F6>', lambda e: toggle_hex_mode())
window.bind('<F7>', lambda e: toggle_hex_space())
window.bind('<Control-s>', lambda e: save_changes_to_file())
window.bind('<MouseWheel>', scroll_callback)
window.bind('<FocusOut>', lambda e: replace_clipboard())
hack_file_text_box.bind('<Control-g>', lambda e: find_jumps())
hack_file_text_box.bind('<Control-f>', lambda e: follow_jump())
# ---------------------------------------------------------------------------
hack_file_text_box.bind('<Control-a>', lambda e: testthing())
# ---------------------------------------------------------------------------
window.bind('<Button-1>', lambda e: (reset_target(),
                                     apply_hack_changes(),
                                     apply_comment_changes(),
                                     window.after(1, lambda: (correct_cursor(e), highlight_stuff(e, skip_moving_cursor=True)))
                                     if disasm else None))

address_input = tk.Text(window, font='Courier', bg=app_config['text_bg_colour'], fg=app_config['text_fg_colour'])
address_output = tk.Text(window, font='Courier', bg=app_config['text_bg_colour'], fg=app_config['text_fg_colour'])
address_translate_button = tk.Button(window, text='Translate Address', command=translate_box,
                                     bg=app_config['text_bg_colour'], activebackground=app_config['text_bg_colour'],
                                     fg=app_config['text_fg_colour'], activeforeground=app_config['text_fg_colour'])

address_input.bind('<Return>', lambda e: window.after(1, lambda: translate_box()))
address_input.bind('<Control-v>', lambda e: window.after(1, lambda: translate_box()))

address_input.place(x=6, y=8, width=85, height=21)
address_translate_button.place(x=95, y=8, height=21)
address_output.place(x=203, y=8, width=85, height=21)

address_text_box.place(x=6, y=35, width=85, height=760)
base_file_text_box.place(x=95, y=35, width=315, height=760)
hack_file_text_box.place(x=414, y=35, width=315, height=760)
comments_text_box.place(x=733, y=35, width=597, height=760)
window.protocol('WM_DELETE_WINDOW', close_window)


open_my_roms_automatically = False
if open_my_roms_automatically:
    window.after(1, lambda: (destroy_change_rom_name_button(), open_files()))

window.mainloop()
