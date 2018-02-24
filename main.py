
import tkinter as tk
import tkinter.font as tkfont
from tkinter import simpledialog, filedialog, colorchooser
import os
import webbrowser
from function_defs import *
from disassembler import Disassembler, REGISTERS_ENCODE, BRANCH_INTS, JUMP_INTS, CIC, DOCUMENTATION


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
    'script_output_dir': working_dir,
    'previous_base_opened': '',
    'previous_hack_opened': '',
    'remember_script': {},
    'remember_batch': {},
    'open_roms_automatically': False,
    'window_geometry': '1133x609',
    'hack_of_base': {},
    'font_size': 10,
    'max_lines': 40,
    'scroll_amount': 8,
    'toggle_base_file': True,
    'immediate_identifier': '$',
    'hex_mode': False,
    'bin_mode': False,
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
    'memory_regions': {},
    'jumps_displaying': {},
    'cursor_line_colour': '#686868',
    'window_background_colour': '#606060',
    'text_bg_colour': '#454545',
    'text_fg_colour': '#D0D0D0',
    'differ_colour': '#E6E000',
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

SCRIPTS_DIR = working_dir + 'script templates\\'
if not exists(SCRIPTS_DIR):
    os.mkdir(SCRIPTS_DIR)
    basic_break_script_file = SCRIPTS_DIR + 'Example Project64d breakpoint.txt'
    if not exists(basic_break_script_file):
        basic_break_script = '\n'.join([
            'events.onexec(0x{{iter}}, function() {',
            '\tdebug.breakhere();',
            '});'
        ])
        with open(basic_break_script_file, 'w') as file:
            file.write(basic_break_script)
    basic_log_counter_script_file = SCRIPTS_DIR + 'Example Project64d log counter.txt'
    if not exists(basic_log_counter_script_file):
        basic_log_counter_script = '\n'.join([
            '{{header}}',
            'var count = [];',
            'count[\'{{iter}}\'] = 0; // only header items containing iter or note will be copied',
            '{{endheader}}',
            '',
            'events.onexec(0x{{iter}}, function() {',
            '\tconsole.log(\'Executing 0x{{iter}}: {{note}}\');',
            '\tconsole.log(\'Count: \' + ++count[\'{{iter}}\']);',
            '});'
        ])
        with open(basic_log_counter_script_file, 'w') as file:
            file.write(basic_log_counter_script)


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
            # if key == 'game_address_mode':
            #     backup_config = unpickle_data('rom disassembler - Copy.config')
            #     config_in_file[key] = backup_config[key].copy()
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
window.geometry(app_config['window_geometry'] + '+0+0')
try:
    window.tk.call('wm', 'iconbitmap', window._w, working_dir + 'n64_disassembler.ico')
except:
    print('Could not load n64_disassembler.ico. Defaulting to tkinter quill.')
    ####
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

disassembly_max_chars = 35
comments_max_chars = 70

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

    r, g, b = get_colours_of_hex(text_bg)
    new_insert_colour = solve_against_greyscale(r, g, b)
    # global colours_window, jumps_window, comments_window
    # global changes_win, opcodes_win, script_win

    labels = []
    listboxes = []
    textboxes = ALL_TEXT_BOXES + [address_input, address_output]
    backgrounds = [window]
    buttons = [address_translate_button, nav_button]
    check_buttons = [auto_copy_checkbtn]

    if phrases_win:
        backgrounds.append(phrases_win)
        listboxes.append(phrases_list_box)
    if hex_win:
        backgrounds.append(hex_win)
        check_buttons.append(hex_win_double_chkbx)
        [textboxes.append(i) for i in [hex_win_float_tbox, hex_win_hex_tbox]]
        [labels.append(i) for i in [hex_win_float_label, hex_win_hex_label]]
    if colours_window:
        backgrounds.append(colours_window)
        labels.append(change_colours_label)
    if jumps_window:
        backgrounds.append(jumps_window)
        [labels.append(i) for i in [functions_label, jumps_label]]
        [listboxes.append(i) for i in [function_list_box, jump_list_box]]
        [check_buttons.append(i) for i in [jumps_comment_checkbox, jumps_hack_checkbox]]
    if comments_window:
        backgrounds.append(comments_window)
        labels.append(comments_filter_label)
        listboxes.append(comments_list_box)
        textboxes.append(filter_text)
        [check_buttons.append(i) for i in [comments_comment_checkbox, comments_hack_checkbox]]
    if changes_win:
        listboxes.append(changes_list_box)
        backgrounds.append(changes_win)
    if opcodes_win:
        backgrounds.append(opcodes_win)
        listboxes.append(codes_list_box)
    if script_win:
        backgrounds.append(script_win)
        listboxes.append(group_list_box)
        [labels.append(i) for i in [groups_label, batch_label, script_label]]
        [textboxes.append(i) for i in [batch_text_box, script_text_box]]
        [buttons.append(i) for i in [script_help_button, plus_button, out_file_button, save_template_button,
                                     load_template_button, view_test_button, groups_refresh_button]]

    [text_box.config(insertbackground=new_insert_colour) for text_box in textboxes]
    [wndw.config(bg=win_bg) for wndw in backgrounds]
    [checkbtn.config(bg=win_bg, fg=text_fg, activebackground=win_bg,activeforeground=text_fg, selectcolor=win_bg)
     for checkbtn in check_buttons]
    [butt.config(bg=win_bg, fg=text_fg, activebackground=text_bg, activeforeground=text_fg) for butt in buttons]
    [widget.config(bg=text_bg, fg=text_fg) for widget in [status_bar] + textboxes + listboxes]
    [label.config(bg=win_bg, fg=text_fg) for label in labels]
    [listbox.config(bg=text_bg, fg=text_fg, selectbackground=win_bg,
                    highlightcolor=text_bg, highlightbackground=text_bg) for listbox in listboxes]

    [label.config(bg=new_tag_config['target']) for label in [target_down_label, target_up_label]]
    [label.config(bg=new_tag_config['jump_from']) for label in [target_of_down_label, target_of_up_label]]
    [label[0].config(bg=app_config['differ_colour']) for label in differ_labels]
    if change_rom_name_button:
        change_rom_name_button.config(bg=text_bg, fg=text_fg, activebackground=text_bg, activeforeground=text_fg)
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


def tags_in_text(filter_text, text):
    tags = filter_text.split(' ')
    for tag in tags:
        if tag not in text:
            return False
    return True


def hex_space(string):
    if not app_config['hex_space_separation']:
        return string
    return ' '.join([string[i:i+2] for i in range(0, len(string), 2)])


def space_bindies(bindie):
    if not app_config['hex_space_separation']:
        return bindie
    return ' '.join([bindie[i:i+8] for i in range(0, len(bindie), 8)])


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

    # differ_labels[i][0].place(x=dif_label_x, y=y_pos, width=2, height=font_h - 6)
    font_w, font_h = font_dimension(main_font_size)
    for i in range(max_lines):
        navi = (navigation + i) << 2
        if int_of_4_byte_aligned_region(disasm.base_file[navi:navi+4]) != int_of_4_byte_aligned_region(disasm.hack_file[navi:navi+4]):
            differ_labels[i][0].place(x=dif_label_x, y=differ_labels[i][1], width=2, height=font_h - 6)
        else:
            differ_labels[i][0].place_forget()

    address = None if c_line else prev_address_target
    for i in range(len(text)):
        navi = navigation + i
        key = str(navi)
        if not app_config['hex_mode'] and not app_config['bin_mode']:
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
            if key in disasm.branches_to:
                hack_file_text_box.tag_add('branch_to',
                                           cursor_value(line, 0),
                                           cursor_value(line + 1, 0))
            navi_offset = navi << 2
            navi_offset = disasm.region_align(navi_offset)
            key2 = str(navi_offset >> 2)
            if key2 in disasm.jumps_to:
                hack_file_text_box.tag_add('jump_to',
                                           cursor_value(line, 0),
                                           cursor_value(line + 1, 0))
            # End if not hex_mode

        # Highlight errors
        if key in user_errors:
            err_code = user_errors[key][0]
            hack_file_text_box.tag_add('bad' if err_code > -3 else 'out_of_range',
                                       cursor_value(i + 1, 0),
                                       cursor_value(i + 2, 0))

    # Highlight jumps/branches to target instruction
    if hack_function:
        reg_offset_key = str(disasm.region_align(prev_cursor_location << 2) >> 2)
        this_key = str(prev_cursor_location)
        dictie = None
        if this_key in disasm.branches_to:
            dictie = disasm.branches_to
        elif reg_offset_key in disasm.jumps_to:
            dictie = disasm.jumps_to
            this_key = reg_offset_key
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
                    # Raises error if user types non-hex characters where an address/offset is
                    address = deci(address)
                except:
                    address = -1
                else:
                    address = disasm.region_unalign(address, game_offset=not disasm.game_address_mode)
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

    targets_lower = [deci(i[:8]) for i in jumps_displaying]
    targets_upper = [deci(i[11:19]) for i in jumps_displaying]
    def find_target_key(target):
        if not disasm.game_address_mode:
            target = disasm.region_unalign(target, game_offset=True)
        else:
            target += disasm.game_offset
        i = 0
        target_key = ''
        while i < len(jumps_displaying):
            if target in range(targets_lower[i], targets_upper[i]):
                target_key = list(jumps_displaying)[i]
                break
            i += 1
        return target_key

    def unmap(decoded_word, navi, int_word):
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
                target_key = find_target_key(target << 2)
                if unmapped_address and target_key:
                    offset_address = navi << 2
                    # offset_address = disasm.region_align(offset_address)
                    if disasm.game_address_mode:
                        offset_address = disasm.region_align(offset_address)
                        offset_address += disasm.game_offset
                    # else:
                    address = extend_zeroes(hexi(offset_address), 8)
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
                            except Exception as e:
                                # print(e)
                                ''

    current_text = get_text_content(hack_file_text_box).upper()
    split_text = current_text.split('\n')
    for i, text in enumerate(split_text):
        try:
            space_in = text.find(' ')
            immid_in = text.rfind(app_config['immediate_identifier'])
            if space_in >= 0 and immid_in >= 0:
                if text[:space_in] in BRANCH_FUNCTIONS + ['J', 'JAL']:
                    address = deci(text[immid_in + 1:])
                    if text[:space_in] in ['J', 'JAL'] and not disasm.game_address_mode:
                        address = disasm.region_align(address)
                    elif text[:space_in] in BRANCH_FUNCTIONS and disasm.game_address_mode:
                        address = disasm.region_unalign(address)
                    new_text = text[:immid_in + 1] + extend_zeroes(hexi(address), 8)
                    split_text[i] = new_text
        except:
            ''
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
        elif is_hex_part or app_config['hex_mode'] or app_config['bin_mode']:
            without_spaces = split_text[i].replace(' ', '')
            try:
                if (len(without_spaces) not in range(0, 9) and app_config['hex_mode']) or \
                        (len(without_spaces) not in range(0, 33) and app_config['bin_mode']):
                    raise Exception()
                int_of = deci(without_spaces) if app_config['hex_mode'] or is_hex_part else dinbies(without_spaces)
            except:
                user_errors[string_key] = (-1, split_text[i])
                continue

            clear_error(string_key)
            if not is_hex_part and int_of != int_word:
                disasm.split_and_store_bytes(int_of, navi)
                unmap(decoded_word, navi, int_word)

        elif not split_text[i]:
            disasm.split_and_store_bytes(0, navi)
            hack_file_text_box.insert(cursor_value(i + 1, 0), 'NOP')
            clear_error(string_key)
            unmap(decoded_word, navi, int_word)

        elif split_text[i] != 'UNKNOWN/NOT AN INSTRUCTION':
            encoded_int = disasm.encode(split_text[i], navi)
            if encoded_int >= 0:
                disasm.split_and_store_bytes(encoded_int, navi)
                clear_error(string_key)
                unmap(decoded_word, navi, int_word)
                if this_word in ['J', 'JAL'] + BRANCH_FUNCTIONS:
                    try:
                        found = split_text[i].rfind(app_config['immediate_identifier'])
                        if found < 0:
                            raise Exception()
                        target = split_text[i][found + 1:]
                        target = deci(target)
                        # target = disasm.region_unalign(target, game_offset=not disasm.game_address_mode)
                        if disasm.game_address_mode:
                            target -= disasm.game_offset
                            str_target = str(target >> 2)
                        else:
                            str_target = str(target >> 2)
                            target = disasm.region_unalign(target, game_offset=True)
                        # print(hexi(target))
                        # print(hexi(int(str_target) << 2))
                        if this_word in ['J', 'JAL']:
                            dic = disasm.jumps_to
                        else:
                            dic = disasm.branches_to
                        mapped_target, mapped_address = disasm.map(dic, navi, str_target)
                        if mapped_address:
                            target_key = find_target_key(target)
                            if target_key:
                                address = navi << 2
                                # print(hexi(address))
                                if disasm.game_address_mode:
                                    address = disasm.region_align(address)
                                    address += disasm.game_offset
                                # else:
                                address = extend_zeroes(hexi(address), 8)
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
        comments_in = comments_list_box.get(0, tk.END)
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
        if not increment:
            hex_navi = extend_zeroes(hexi(navi << 2), 8)
        else:
            hex_navi = extend_zeroes(hexi(disasm.region_align(navi << 2) + increment), 8)
        if not split_text[i] and string_key in disasm.comments:
            del disasm.comments[string_key]
            if comments_window:
                if hex_navi in addresses_dict:
                    comments_list_box.delete(addresses_dict[hex_navi])
            if hex_navi in orig_keys:
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

        if string_key in disasm.comments:
            if split_text[i] == disasm.comments[string_key]:
                continue
        disasm.comments[string_key] = split_text[i]

        if comments_window:
            if hex_navi in addresses_dict:
                comments_list_box.delete(addresses_dict[hex_navi])
                if tags_in_text(filtering, split_text[i].lower()):
                    comments_list_box.insert(addresses_dict[hex_navi], '{}: {}'.format(hex_navi, split_text[i]))
            else:
                this_int_address = navi << 2
                if disasm.game_address_mode:
                    this_int_address += disasm.game_offset
                target = -1
                for j in range(len(int_addresses)):
                    if this_int_address < int_addresses[j]:
                        target = j
                        break
                if tags_in_text(filtering, split_text[i].lower()):
                    if target >= 0:
                        comments_list_box.insert(target, '{}: {}'.format(hex_navi, split_text[i]))
                    else:
                        comments_list_box.insert(tk.END, '{}: {}'.format(hex_navi, split_text[i]))

        if hex_navi in orig_keys:
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
    # save_config()  Dont know why this was here


def buffer_append(buffer):
    if buffer is hack_buffer:
        tuple = (navigation,
                 get_cursor(hack_file_text_box)[0],
                 get_text_content(hack_file_text_box),
                 max_lines,
                 app_config['immediate_identifier'],
                 disasm.game_address_mode,
                 app_config['hex_mode'],
                 app_config['bin_mode'])
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
# def replace_clipboard():
#     global clipboard
#     try:
#         window.clipboard_get()
#         clipboard = ''
#     except:
#         if clipboard:
#             window.clipboard_append(clipboard)


# Textbox behaviour upon key presses
# clipboard = ''
def keyboard_events(handle, max_char, event, buffer = None, hack_function = False):
    # global clipboard
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
    bad_bad_hotkey = ctrl_held and up_keysym in ['Z', 'T', 'O']
    if bad_bad_hotkey:
        # Messes with custom behaviour, so wait until after changes are made
        #   by bad hotkey, then restore text box to how it was before the hotkey
        window.after(0, lambda: (handle.delete('1.0', tk.END),
                                 handle.insert('1.0', joined_text),
                                 handle.mark_set(tk.INSERT, cursor),
                                 highlight_stuff()))
        if up_keysym == 'O' and hack_function:
            optimise_function()
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
                bin_mode = buffer[1][place][7]
                if immediate_id != app_config['immediate_identifier']:
                    app_config['immediate_identifier'] = immediate_id
                    save_config()
                    disasm.immediate_identifier = immediate_id
                if game_address_mode != disasm.game_address_mode:
                    # disasm.game_address_mode = game_address_mode
                    # app_config['game_address_mode'][disasm.hack_file_name] = game_address_mode
                    # save_config()
                    toggle_address_mode(buffering=False)
                if hex_mode != app_config['hex_mode']:
                    app_config['hex_mode'] = hex_mode
                    save_config()
                elif bin_mode != app_config['bin_mode']:
                    app_config['bin_mode'] = bin_mode
                    save_config()

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
            # if sel_end_column == 0:
            #     selection_line_mod = True
            #     selection_end, sel_end_line, sel_end_column = modify_cursor(selection_end, -1, 0, split_text)
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

    try:
        clipboard = window.clipboard_get()
    except:
        clipboard = ''

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
    # A work around is required in order for the code to be able to serve it's intended purpose
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
            # clipboard = selection_text
            window.after(0, lambda: window.clipboard_clear())
            window.after(0, lambda: window.clipboard_append(selection_text))

        if selection_removable:
            handle.delete(selection_start, selection_end)
            lines_diff += selection_lines
            handle.mark_set(tk.INSERT, selection_start)

    if is_pasting:
        # If window clipboard has contents, contents to be drawn from there, else draw from clipboard
        # window clipboard having contents means user has copied data from an external source
        try:
            clipboard = window.clipboard_get()
        except:
            clipboard = ''
        if clipboard:
            if '\n' in clipboard:
                # Ensure the text has within the maximum amount of lines and columns
                split_clip = clipboard.split('\n')
                line_boundary = max_lines - (sel_start_line if has_selection else line)
                split_clip = split_clip[:line_boundary + 1]
                split_clip = [i[:max_char] for i in split_clip]
                lines_diff -= len(split_clip) - 1
                clipboard = '\n'.join(split_clip)
                if not selection_function:
                    min_del, _, __ = modify_cursor(cursor, 0, 'min', split_text)
                    max_del, _, __ = modify_cursor(cursor, -lines_diff, 'max', split_text)
                    handle.delete(min_del, max_del)
                    lines_diff = 0
            paste_text = clipboard
            # window.after(1, lambda: (window.clipboard_clear(), window.clipboard_append(winnie_clip)))

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
            window.clipboard_append(paste_text)
        window.clipboard_clear()
        handle.insert(insertion_place, paste_text)
        if is_pasting:
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
        def find_line_chars(text, max_char):
            text_list = text.split('\t')
            if len(text_list) == 1:
                return text[:max_char - 1], len(text)
            new_text = text_list[0] + '\t'
            line_chars = len(text_list[0])
            prev_not = not text_list[0]
            for i, txt in enumerate(text_list[1:]):
                txtlen = len(txt)
                if not txt:
                    if not line_chars % 8 and not prev_not:
                        line_chars += 1
                    else:
                        line_chars += 8 - (line_chars % 8)
                    prev_not = True
                else:
                    line_chars += txtlen + (8 - (line_chars % 8))
                    if line_chars > max_char - 1:
                        new_text += txt[:(max_char - 1) - line_chars]
                    prev_not = False
                if line_chars < max_char:
                    new_text += txt
                    if i != len(text_list) - 2:
                        new_text += '\t'
            return new_text, line_chars
        new_text, line_chars = find_line_chars(text=split_text[line - 1], max_char=max_char)
        if line_chars > max_char - 1:
            handle.delete(cursor_value(line, 0),
                          modify_cursor(cursor, 0, 'max', split_text)[0])
            handle.insert(cursor_value(line, 0), new_text)
        if hack_function and event.keysym == event.keysym.lower():
            def replace_with_upper(event, cursor):
                new_cursor = modify_cursor(cursor, 0, 1, get_text_content(hack_file_text_box))[0]
                hack_file_text_box.delete(cursor, new_cursor)
                hack_file_text_box.insert(cursor, event.char.upper())
            window.after(0, lambda: replace_with_upper(event, cursor))

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
    keyboard_events(base_file_text_box, disassembly_max_chars, event, buffer=False))

hack_file_text_box.bind('<Key>', lambda event:
    keyboard_events(hack_file_text_box, disassembly_max_chars, event, buffer=hack_buffer, hack_function=True))

comments_text_box.bind('<Key>', lambda event:
    keyboard_events(comments_text_box, comments_max_chars, event, buffer=comments_buffer))


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


def navigate_to(index, center=False, widget=None, region_treatment=False, region_override=False):
    global navigation, change_rom_name_button, prev_cursor_location
    if not disassembler_loaded():
        return
    destroy_change_rom_name_button()
    if (region_treatment and disasm.game_address_mode) or region_override:
        index = disasm.region_unalign(index << 2, invert=False, game_offset=True) >> 2
    indexed = index
    if center:
        index -= max_lines >> 1
    shift_amount = navigation

    # Correct the navigation if traveling out of bounds, also calculate limits for file samples to display
    amount_words = disasm.file_length >> 2
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

    # Calculate what addresses to display in the address box
    addresses = [i * 4 for i in range(navigation, limit)]
    if disasm.game_address_mode:
        for i, address in enumerate(addresses):
            in_region = False
            for reg in disasm.memory_regions:
                rom_start = reg[0] - reg[2]
                rom_end = (reg[0] + reg[1]) - reg[2]
                if address in range(rom_start, rom_end):
                    addresses[i] += reg[2]
                    in_region = True
                    break
            if not in_region:
                addresses[i] += disasm.game_offset
    address_range = [extend_zeroes(hexi(i), 8) for i in addresses]

    hex_or_bin = app_config['hex_mode'] or app_config['bin_mode']

    # Disassemble ints into instructions, or display as hex or bin
    base_disassembled = []
    hack_disassembled = []
    for i in range(len(ints_in_base_sample)):
        if navigation + i < 16 or app_config['hex_mode']:
            base_disassembled.append(hex_space(extend_zeroes(hexi(ints_in_base_sample[i]), 8)))
            hack_disassembled.append(hex_space(extend_zeroes(hexi(ints_in_hack_sample[i]), 8)))
        elif app_config['bin_mode']:
            base_disassembled.append(space_bindies(extend_zeroes(bindies(ints_in_base_sample[i]), 32)))
            hack_disassembled.append(space_bindies(extend_zeroes(bindies(ints_in_hack_sample[i]), 32)))
        else:
            base_disassembled.append(disasm.decode(ints_in_base_sample[i], navigation + i))
            hack_disassembled.append(disasm.decode(ints_in_hack_sample[i], navigation + i))

    # base_disassembled = [disasm.decode(ints_in_base_sample[i], navigation + i)
    #                      if navigation + i > 15 and not hex_or_bin else
    #                      hex_space(extend_zeroes(hexi(ints_in_base_sample[i]), 8))
    #                      for i in range(len(ints_in_base_sample))]
    # hack_disassembled = [disasm.decode(ints_in_hack_sample[i], navigation + i)
    #                      if navigation + i > 15 and not hex_or_bin else
    #                      hex_space(extend_zeroes(hexi(ints_in_hack_sample[i]), 8))
    #                      for i in range(len(ints_in_hack_sample))]

    # Adjust branches or jumps to offset to custom regions
    for disassembly in [base_disassembled, hack_disassembled] if not hex_or_bin else []:
        for i, text in enumerate(disassembly):
            try:
                space_in = text.find(' ')
                immid_in = text.rfind(app_config['immediate_identifier'])
                if space_in >= 0 and immid_in >= 0:
                    if text[:space_in] in BRANCH_FUNCTIONS + ['J', 'JAL']:
                        address = deci(text[immid_in + 1:])
                        if text[:space_in] in ['J', 'JAL'] and not disasm.game_address_mode:
                            address = disasm.region_unalign(address, game_offset=True)
                        elif text[:space_in] in BRANCH_FUNCTIONS and disasm.game_address_mode:
                            address = disasm.region_align(address, game_offset=True)
                        new_text = text[:immid_in + 1] + extend_zeroes(hexi(address), 8)
                        disassembly[i] = new_text
            except Exception as e:
                print(e)

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
        address >>= 2
    except:
        if widget:
            widget.focus_force()
        return
    apply_hack_changes()
    apply_comment_changes()
    reset_target()
    navigate_to(address, center=True, widget=widget, region_treatment=True)
    if widget:
        widget.focus_force()



def navigation_prompt(root=window):
    if not disassembler_loaded():
        return
    address = simpledialog.askstring('Navigate to address', '', parent=root)
    if not address:
        return
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
    if app_config['calc_crc'][disasm.hack_file_name]:
        status_text.set('Calculating checksum...')
        window.update()
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
        app_config['memory_regions'][new_file_name] = app_config['memory_regions'][disasm.hack_file_name].copy()
        app_config['remember_batch'][new_file_name] = app_config['remember_batch'][disasm.hack_file_name]
        app_config['remember_script'][new_file_name] = app_config['remember_script'][disasm.hack_file_name]
        disasm.hack_file_name = new_file_name
        disasm.comments_file = new_file_path + ' comments.txt'
        disasm.jumps_file = new_file_path + ' jumps.data'

        window.title('ROM Disassembler - ' + disasm.hack_file_name)

    app_config['CIC'][disasm.hack_file_name] = disasm.cic
    app_config['jumps_displaying'][disasm.hack_file_name] = jumps_displaying.copy()
    app_config['game_address_mode'][disasm.hack_file_name] = disasm.game_address_mode
    save_config()
    with open(disasm.jumps_file, 'wb') as jumps_file:
        dump((disasm.jumps_to, disasm.branches_to, disasm.jalr_list), jumps_file)

    with open(disasm.hack_folder + disasm.hack_file_name, 'wb') as file:
        file.write(disasm.hack_file)

    _filename = disasm.comments_file + '(Backup '
    if exists(disasm.comments_file):
        i = 0
        while True:
            i += 1
            if not exists(_filename + str(i) + ').txt'):
                _filename += str(i) + ').txt'
                with open(_filename, 'w') as backup_comments_file:
                    with open(disasm.comments_file, 'r') as comments_file:
                        backup_comments_file.write(comments_file.read())
                break

    try:
        with open(disasm.comments_file, 'w') as file:
            file.write(dict_to_string(disasm.comments))
        if _filename != disasm.comments_file + '(Backup ':
            os.remove(_filename)
    except Exception as e:
        simpledialog.messagebox._show('Error', 'There was trouble saving your comments file. '
                                               'A backup of your old comments can be found next to the original comments file. '
                                               'Your rom file was saved without error.'
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
    global changes_win, opcodes_win, script_win, phrases_win, mem_regions_win, hex_win
    if changes_win:
        changes_win.destroy()
        changes_win = None
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
    if script_win:
        script_win.destroy()
        script_win = None
    if phrases_win:
        phrases_win.destroy()
        phrases_win = None
    if mem_regions_win:
        mem_regions_win.destroy()
        mem_regions_win = None
    if not not_main:
        if hex_win:
            hex_win.destroy()
            hex_win = None
        if colours_window:
            colours_window.destroy()
            colours_window = None
        if opcodes_win:
            opcodes_win.destroy()
            opcodes_win = None
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
    close_win.resizable(False, False)
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
                                                             'Bypass CRC". If that doesn\'t work and '
                                                             'fiddling around with manually setting the '
                                                             'CIC chip also doesn\'t work, then I\'m sorry, '
                                                             'I dunno man.')
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
        if disasm.hack_file_name not in app_config['memory_regions']:
            app_config['memory_regions'][disasm.hack_file_name] = []
        else:
            disasm.memory_regions = app_config['memory_regions'][disasm.hack_file_name].copy()
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
        if disasm.hack_file_name not in app_config['remember_script']:
            app_config['remember_script'][disasm.hack_file_name] = ''
        if disasm.hack_file_name not in app_config['remember_batch']:
            app_config['remember_batch'][disasm.hack_file_name] = ''
        jumps_displaying = app_config['jumps_displaying'][disasm.hack_file_name].copy()
        time_taken = timer_get()

        if time_taken > 100:
            simpledialog.messagebox._show('Woah!', 'That took {} seconds.'.format(time_taken))

    # Otherwise text boxes sometimes don't get updated to notify user of jump mapping
    window.after(1, rest_of_function)


def remap_jumps():
    if not disassembler_loaded():
        return

    class custom_tbox():
        def __init__(self, statustext, update):
            self.update = update
            self.tbox = statustext
        def insert(self, index, text):
            self.tbox.set(text)
        def delete(self, start, end):
            ''

    disasm.map_jumps(custom_tbox(status_text, status_bar.update), skip_loading=True)
    status_text.set('100%')
    status_bar.update()


# def find_largest_decodable_sections():
#     if not disassembler_loaded():
#         return
#     # undecodables = disasm.map_jumps(text_box=address_text_box,skip_loading=True)
#
#     def test_map():
#         f_len = disasm.file_length
#         decodables = []
#         i = 0
#         decodable = 0
#         while i < f_len:
#             decoded = disasm.decode(int_of_4_byte_aligned_region(disasm.hack_file[i:i+4]), i >> 2)
#             if decoded:
#                 decodable += 1
#             else:
#                 if decodable:
#                     decodables.append([
#                         extend_zeroes(hexi(i - (decodable << 2)), 8),
#                         extend_zeroes(hexi(i - 4), 8)
#                     ])
#                 decodable = 0
#             i += 4
#         return decodables
#
#     decodables = test_map()
#     remap_win = tk.Tk()
#     remap_win.title('Decodable sections')
#     remap_win.geometry('500x800+50+50')
#     sects_list = tk.Listbox(remap_win, font=('Courier', main_font_size))
#
#     length_reference = {}
#     length_keys = []
#     keys_add = lambda i: '' if i in length_keys else length_keys.append(i)
#     [keys_add(deci(i[1]) - deci(i[0])) for i in decodables]
#     for i in reversed(sorted(length_keys)):
#         length_reference[str(i)] = []
#
#     for addr_range in decodables:
#         key = str(deci(addr_range[1]) - deci(addr_range[0]))
#         length_reference[key].append(addr_range[0] + ' - ' + addr_range[1] + '  ||  Length: 0x' + hexi(int(key)))
#
#     for key in length_reference:
#         for entry in length_reference[key]:
#             sects_list.insert(tk.END, entry)
#
#     def list_box_callback():
#         curselect = sects_list.curselection()
#         if not curselect:
#             return
#         address = sects_list.get(curselect[0])[:8]
#         navi = deci(address) >> 2
#         navigate_to(navi, center=True, widget=None)
#
#     sects_list.bind('<<ListboxSelect>>', lambda _: list_box_callback())
#     sects_list.place(x=5, y=5, width=490, height=790)
#     remap_win.resizable(False, False)
#     remap_win.mainloop()


def toggle_address_mode(buffering=True):
    global function_select
    if not disassembler_loaded():
        return
    apply_hack_changes()
    apply_comment_changes()
    cursor, line, column = get_cursor(hack_file_text_box)
    if buffering:
        buffer_append(hack_buffer)
    toggle_to = not disasm.game_address_mode
    disasm.game_address_mode = toggle_to
    # This causes bugs, have it only save when user saves
    # app_config['game_address_mode'][disasm.hack_file_name] = toggle_to
    # save_config()
    navigate_to(navigation)
    hack_file_text_box.mark_set(tk.INSERT, cursor)
    highlight_stuff(skip_moving_cursor=True)
    increment = disasm.game_offset if toggle_to else -disasm.game_offset
    config_data = jumps_displaying.copy()

    def fix_listbox(listbox, see):
        list_contents = listbox.get(0, tk.END)
        list_addresses = [deci(i[:8]) if i else 0 for i in list_contents]
        realigned_addresses = [0 if not i else (disasm.region_align(i) if toggle_to else disasm.region_unalign(i)) for i in list_addresses]
        if listbox is phrases_list_box:
            addresses = [extend_zeroes(hexi(i + increment), 8) if i else '' for i in realigned_addresses]
        else:
            addresses = [extend_zeroes(hexi(i + increment), 8) for i in realigned_addresses]
        listbox.delete(0, tk.END)
        for i, address in enumerate(addresses):
            if address or not listbox is phrases_list_box:
                listbox.insert(tk.END, '{}{}'.format(address, list_contents[i][8:]))
            else:
                if phrases_win:
                    if listbox is phrases_list_box:
                        listbox.insert(tk.END, '')
        listbox.see(see)

    if comments_window:
        fix_listbox(comments_list_box, comments_curselection)

    if changes_win:
        fix_listbox(changes_list_box, changes_curselect)

    if phrases_win:
        fix_listbox(phrases_list_box, phrases_curselect)

    for key in config_data:
        del jumps_displaying[key]
        addy_1 = deci(key[:8])
        addy_2 = deci(key[11:19])
        addy_1 = (disasm.region_align(addy_1) if toggle_to else disasm.region_unalign(addy_1)) + increment
        addy_2 = (disasm.region_align(addy_2) if toggle_to else disasm.region_unalign(addy_2)) + increment
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
            addy_1 = deci(function_select[:8])
            addy_2 = deci(function_select[11:19])
            comment = function_select[19:]
            addy_1 = (disasm.region_align(addy_1) if toggle_to else disasm.region_unalign(addy_1)) + increment
            addy_2 = (disasm.region_align(addy_2) if toggle_to else disasm.region_unalign(addy_2)) + increment
            addy_1 = extend_zeroes(hexi(addy_1), 8)
            addy_2 = extend_zeroes(hexi(addy_2), 8)
            function_select = '{} - {}{}'.format(addy_1, addy_2, comment)
        function_list_box.delete(0, tk.END)
        jump_box = jump_list_box.get(0,tk.END)
        jump_list_box.delete(0, tk.END)
        for key in jumps_displaying:
            function_list_box.insert(tk.END, key)
        for i in jump_box:
            value = deci(i[:8])
            value = (disasm.region_align(value) if toggle_to else disasm.region_unalign(value)) + increment
            value = extend_zeroes(hexi(value), 8)
            jump_list_box.insert(tk.END, value + i[8:])
        if function_curselect:
            function_list_box.see(function_curselect)
        if jumps_curselect:
            jump_list_box.see(jumps_curselect)


def toggle_hex_mode(buffering=True):
    apply_hack_changes()
    apply_comment_changes()
    cursor, line, column = get_cursor(hack_file_text_box)
    if buffering:
        buffer_append(hack_buffer)
    app_config['hex_mode'] = not app_config['hex_mode']
    if app_config['hex_mode']:
        app_config['bin_mode'] = False
    save_config()
    navigate_to(navigation)
    hack_file_text_box.mark_set(tk.INSERT, modify_cursor(cursor, 0, 'max', get_text_content(hack_file_text_box))[0])
    highlight_stuff(skip_moving_cursor=True)


def toggle_bin_mode(buffering=True):
    apply_hack_changes()
    apply_comment_changes()
    cursor, line, column = get_cursor(hack_file_text_box)
    if buffering:
        buffer_append(hack_buffer)
    app_config['bin_mode'] = not app_config['bin_mode']
    if app_config['bin_mode']:
        app_config['hex_mode'] = False
    save_config()
    navigate_to(navigation)
    hack_file_text_box.mark_set(tk.INSERT, modify_cursor(cursor, 0, 'max', get_text_content(hack_file_text_box))[0])
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
                        '=', '+', '-', '_', '&', '^', '%', '$', '#',
                        '@', '!', '`', '~', '/', '?']
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


opcodes_win = codes_list_box = None
def opcodes_list():
    global opcodes_win, codes_list_box
    if opcodes_win:
        opcodes_win.deiconify()
        opcodes_win.focus_force()
        return
    def opcodes_win_equals_none():
        global opcodes_win
        opcodes_win.destroy()
        opcodes_win = None
    opcodes_win = tk.Tk()
    opcodes_win.title('Opcodes list')
    opcodes_win.geometry('664x800+50+50')
    scrollbar = tk.Scrollbar(opcodes_win)
    scrollbar.pack(side=tk.RIGHT, fill=tk.Y)
    codes_list_box = tk.Listbox(opcodes_win, font=('Courier', 10),yscrollcommand=scrollbar.set)
    [codes_list_box.insert(tk.END, i + ': ' + DOCUMENTATION[i]) for i in DOCUMENTATION]
    codes_list_box.place(x=5, y=5, width=640, height=790)
    opcodes_win.bind('<Escape>', lambda _: opcodes_win.destroy())
    change_colours()
    opcodes_win.protocol('WM_DELETE_WINDOW', opcodes_win_equals_none)
    opcodes_win.resizable(False, False)
    scrollbar.config(command=codes_list_box.yview)
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
    about.resizable(False, False)
    about.mainloop()


jumps_window = None
function_list_box = None
jump_list_box = None
jumps_label = None
functions_label = None
jumps_comment_checkbox = jumps_hack_checkbox = None
function_select = ''
jumps_displaying = {}
function_curselect = 0
jumps_curselect = 0
def find_jumps(just_window=False):
    global function_select, jumps_window, function_list_box, jump_list_box, jumps_label, functions_label, \
        jumps_comment_checkbox, jumps_hack_checkbox, function_curselect, jumps_curselect
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
                jumps_hack_checkbox.deselect()
            else:
                app_config['jumps_auto_focus_comments'] = False
                app_config['jumps_auto_focus_hack'] = True
                jumps_hack_checkbox.select()
                jumps_comment_checkbox.deselect()
            save_config()

        def comments_checkbox_callback():
            if app_config['jumps_auto_focus_comments']:
                app_config['jumps_auto_focus_comments'] = False
                jumps_comment_checkbox.deselect()
            else:
                app_config['jumps_auto_focus_hack'] = False
                app_config['jumps_auto_focus_comments'] = True
                jumps_comment_checkbox.select()
                jumps_hack_checkbox.deselect()
            save_config()

        jumps_hack_checkbox = tk.Checkbutton(jumps_window, text='Auto-focus hack textbox',
                                       command=lambda: window.after(1, lambda: hack_checkbox_callback()))
        jumps_comment_checkbox = tk.Checkbutton(jumps_window, text='Auto-focus comments textbox',
                                          command=lambda: window.after(1, lambda: comments_checkbox_callback()))
        jumps_hack_checkbox.place(x=100, y=6)
        jumps_comment_checkbox.place(x=294, y=6)
        if app_config['jumps_auto_focus_comments']:
            jumps_comment_checkbox.select()
        elif app_config['jumps_auto_focus_hack']:
            jumps_hack_checkbox.select()

        def function_list_callback():
            global function_select, function_curselect, jumps_curselect
            curselect = function_list_box.curselection()
            if not curselect:
                return
            jumps_curselect = 0
            function_curselect = curselect[0]
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
            if_disasm = disasm.game_address_mode
            navigate_to(start_address + increment, center=True, widget=widget, region_treatment=if_disasm, region_override=if_disasm)
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
            global jumps_curselect
            curselect = jump_list_box.curselection()
            if not curselect:
                return
            jumps_curselect = curselect[0]
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
            if_disasm = disasm.game_address_mode
            navigate_to(navi, center=True, widget=widget, region_treatment=if_disasm, region_override=if_disasm)
            if widget:
                widget.focus_force()

        function_list_box.bind('<<ListboxSelect>>', lambda _: function_list_callback())
        jump_list_box.bind('<<ListboxSelect>>', lambda _: jump_list_callback())
        function_list_box.bind('<Key>', function_list_key)
        for key in jumps_displaying:
            function_list_box.insert(tk.END,key)
        functions_label = tk.Label(jumps_window, text='Functions')
        jumps_label = tk.Label(jumps_window, text='Jumps to Function')
        function_list_box.place(x=func_list_x,y=func_list_y,width=func_list_w,height=func_list_h)
        jump_list_box.place(x=jumps_list_x,y=jumps_list_y,width=jumps_list_w,height=jumps_list_h)
        functions_label.place(x=6,y=5)
        jumps_label.place(x=jumps_list_x,y=jumps_label_y)
        def jumps_window_equals_none():
            global jumps_window, function_select, jumps_curselect, function_curselect
            jumps_curselect = function_curselect = 0
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
        jumps_window.resizable(False, False)
        jumps_window.after(2, lambda: change_colours())
        jumps_window.after(1, lambda: jumps_window.mainloop())
    elif jumps or just_window:
        jumps_window.deiconify()
        jumps_window.focus_force()
    if just_window:
        return
    key = extend_zeroes(hexi(function_start << 2),8) + ' - ' + extend_zeroes(hexi(function_end << 2),8)
    key_not_in_jumps_displaying = True
    config = jumps_displaying.copy()
    increment = -disasm.game_offset if disasm.game_address_mode else 0
    try:
        comment_key = str((deci(key[:8]) if not disasm.game_address_mode else (disasm.region_unalign(deci(key[:8])) + increment)) >> 2)
    except ValueError as e:
        # User attempting to get jumps from the top of the header
        return
    is_in_comments = comment_key in disasm.comments
    for display_key in config:
        key_not_in_jumps_displaying = key_not_in_jumps_displaying and display_key[:19] != key
        if not key_not_in_jumps_displaying:
            if is_in_comments:
                new_key = key + ' ' + disasm.comments[comment_key]
            else:
                new_key = key
            if new_key != display_key:
                del jumps_displaying[display_key]
                key_not_in_jumps_displaying = True
                # key = new_key
    if is_in_comments:
        key += ' {}'.format(disasm.comments[comment_key])
    if key_not_in_jumps_displaying and jumps:
        for i in range(len(jumps)):
            comment_key = str((deci(jumps[i]) + increment) >> 2)
            if comment_key in disasm.comments:
                jumps[i] += ' ' + disasm.comments[comment_key]
        jumps_displaying[key] = jumps
        save_config()
        if jumps_window:
            function_list_box.insert(tk.END, key)


comments_window = None
comments_list_box = None
filter_text = None
comments_filter_label = None
comments_hack_checkbox = None
comments_comment_checkbox = None
comments_curselection = 0
def view_comments():
    global comments_window, comments_list_box, filter_text, comments_filter_label, comments_hack_checkbox, \
        comments_comment_checkbox, comments_curselection
    if not disassembler_loaded():
        return
    if comments_window:
        comments_window.deiconify()
        filter_text.focus_force()
        return
    comments_window = tk.Tk()
    comments_window.title('Comments')
    comments_window.geometry('{}x{}'.format(comments_win_w,comments_win_h))
    comments_list_box = tk.Listbox(comments_window, font=('Courier', main_font_size))
    comments_list_box.place(x=comments_x,y=comments_y,width=comments_w,height=comments_h)

    # If this doesn't want to work the way the auto-copy checkbox does, then it will have to work by force
    def hack_checkbox_callback():
        if app_config['comments_auto_focus_hack']:
            app_config['comments_auto_focus_hack'] = False
            comments_hack_checkbox.deselect()
        else:
            app_config['comments_auto_focus_comments'] = False
            app_config['comments_auto_focus_hack'] = True
            comments_hack_checkbox.select()
            comments_comment_checkbox.deselect()
        save_config()

    def comments_checkbox_callback():
        if app_config['comments_auto_focus_comments']:
            app_config['comments_auto_focus_comments'] = False
            comments_comment_checkbox.deselect()
        else:
            app_config['comments_auto_focus_hack'] = False
            app_config['comments_auto_focus_comments'] = True
            comments_comment_checkbox.select()
            comments_hack_checkbox.deselect()
        save_config()

    def increment():
        if disasm.game_address_mode:
            return disasm.game_offset
        return 0

    def populate_list():
        if comments_window:
            comments_list_box.delete(0, tk.END)
            filtering = filter_text.get('1.0', tk.END).split('\n')[0].lower()
            for key in sorted([int(key) for key in disasm.comments
                               if tags_in_text(filtering, disasm.comments[key].lower())][:5000]):
                address = key << 2
                if disasm.game_address_mode:
                    address = disasm.region_align(address)
                comments_list_box.insert(tk.END, '{}: {}'.format(extend_zeroes(hexi(address + increment()), 8),
                                                                 disasm.comments[str(key)]))

    comments_hack_checkbox = tk.Checkbutton(comments_window, text='Auto-focus hack textbox',
                                   command=lambda: window.after(1, lambda: hack_checkbox_callback()))
    comments_comment_checkbox = tk.Checkbutton(comments_window, text='Auto-focus comments textbox',
                                      command=lambda: window.after(1, lambda: comments_checkbox_callback()))
    filter_text = tk.Text(comments_window, font=('Courier', 10))
    comments_hack_checkbox.place(x=6, y=6)
    comments_comment_checkbox.place(x=175, y=6)
    filter_text.place(x=370, y=9, width=font_dimension(10)[0] * 20, height=20)
    comments_filter_label = tk.Label(comments_window, text='Filter list')
    comments_filter_label.place(x=535,y=9)
    filter_text.bind('<Key>', lambda _: window.after(1, populate_list))
    if app_config['comments_auto_focus_comments']:
        comments_comment_checkbox.select()
    elif app_config['comments_auto_focus_hack']:
        comments_hack_checkbox.select()

    populate_list()

    def comments_list_callback(event):
        global comments_curselection
        curselect = comments_list_box.curselection()
        if not curselect:
            return
        comments_curselection = curselect[0]
        apply_hack_changes()
        apply_comment_changes()
        navi = deci(comments_list_box.get(curselect[0])[:8]) >> 2
        if disasm.game_address_mode:
            navi -= disasm.game_offset >> 2
        reset_target()
        widget = None
        if app_config['comments_auto_focus_comments']:
            widget = comments_text_box
        elif app_config['comments_auto_focus_hack']:
            widget = hack_file_text_box
        ifdisasm = disasm.game_address_mode
        navigate_to(navi, center=True, widget=widget, region_treatment=ifdisasm, region_override=ifdisasm)
        if widget:
            widget.focus_force()
        else:
            filter_text.focus_force()

    comments_list_box.bind('<<ListboxSelect>>', comments_list_callback)

    def comments_window_equals_none():
        global comments_window, comments_curselection
        comments_curselection = 0
        comments_window.destroy()
        comments_window = None

    def focus_in(event):
        if not event.widget is comments_list_box:
            filter_text.focus_force()

    comments_window.bind('<Escape>', lambda e: comments_window_equals_none())
    comments_window.bind('<FocusIn>', focus_in)
    comments_window.bind('<F1>', lambda e: filter_text.focus_force())
    comments_window.bind('<F2>', lambda e: find_jumps(just_window=True))
    comments_window.bind('<F3>', lambda e: toggle_base_file())
    comments_window.bind('<F4>', lambda e: navigation_prompt(root=comments_window))
    comments_window.bind('<F5>', lambda e: toggle_address_mode())
    comments_window.bind('<Control-s>', lambda e: save_changes_to_file())
    comments_window.bind('<Control-S>', lambda e: save_changes_to_file())
    comments_window.protocol('WM_DELETE_WINDOW', comments_window_equals_none)
    filter_text.focus_force()
    change_colours()
    comments_window.resizable(False, False)
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
        address = disasm.region_align(disasm.solve_address(navi, int_word & 0x03FFFFFF)) >> 2
    elif BRANCH_INTS[opcode]:
        address = sign_16_bit_value(int_word & 0xFFFF) + navi
    if address:
        text_box_contents = get_text_content(hack_file_text_box)
        # (navigation, cursor_location, text_box_content, immediate_id, game_address_mode)
        frame = hack_buffer[1][hack_buffer[0]]
        if frame[0] != navigation or frame[1] != cursor or frame[2] != text_box_contents:
            buffer_append(hack_buffer)
        navigate_to(address, center=True, region_treatment=True, region_override=True)
        buffer_append(hack_buffer)


colours_window = change_colours_label = None
def set_colour_scheme():
    global colours_window, change_colours_label
    if colours_window:
        return
    if not disassembler_loaded():
        simpledialog.messagebox._show('Please note', 'You will need to open rom files in order to see your text '
                                                     'colour changes.')
    colours_window = tk.Tk()
    colours_window.title('Colour scheme')
    colours_window.geometry('{}x{}'.format(458, 516))

    def colour_buttons_callback(which, with_colour=''):
        if not with_colour:
            if which in app_config['tag_config']:
                start_colour = app_config['tag_config'][which]
            else:
                start_colour = app_config[which]
            new_colour = colorchooser.askcolor(color=start_colour, parent=colours_window)
            print(new_colour)
        else:
            r, g, b = get_colours_of_hex(with_colour)
            new_colour = ((r, g, b), with_colour)
        if new_colour[0] is None:
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
        'differ_colour': 'Modified code labels',
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
        'differ_colour': '#ff8d1c',
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
    custom_buttons['differ_colour'].config(command=lambda: colour_buttons_callback('differ_colour'))
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
    previous_setting_buttons['differ_colour'].config(command=lambda: colour_buttons_callback('differ_colour',current_colours['differ_colour']))
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
    default_dark_buttons['differ_colour'].config(command=lambda: colour_buttons_callback('differ_colour',dark_colours['differ_colour']))
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
    default_bright_buttons['differ_colour'].config(command=lambda: colour_buttons_callback('differ_colour',bright_colours['differ_colour']))
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
    change_colours_label = tk.Label(colours_window, text='Customise')
    change_colours_label.place(x=85,y=10)
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
    change_colours()
    colours_window.resizable(False, False)
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


def set_widget_sizes(new_size=0, new_max_lines=0):
    global main_font_size, max_lines, top_label_x, top_label_w, bot_label_x, bot_label_w
    global top_label_y, bot_label_y, comments_win_h, comments_x, comments_y, comments_w
    global comments_h, jumps_win_w, jumps_win_h, func_list_w, func_list_y, func_list_x
    global func_list_h, jumps_list_x, jumps_list_y, jumps_list_w, jumps_list_h, jumps_label
    global jumps_label_y, comments_win_w, targ_top_label_x, targ_top_label_y, targ_top_label_w
    global targ_bot_label_x, targ_bot_label_y, targ_bot_label_w, dif_label_x
    # if disassembler_loaded():
    #     apply_comment_changes()
    #     apply_hack_changes()
    window.update_idletasks()
    old_max_lines = max_lines
    if new_size:
        main_font_size = new_size
    if new_max_lines:
        max_lines = new_max_lines
    font_w, font_h = font_dimension(main_font_size)
    widget_y = 35
    widget_h = (max_lines * font_h) + 4
    win_w, win_h, win_x, win_y = geometry(window.geometry())
    x_1 = 6
    w_1 = (font_w * 8) + 6
    w_2 = (font_w * disassembly_max_chars) + 6
    if app_config['toggle_base_file']:
        x_2 = x_1 + w_1 + 4
        x_3 = x_2 + w_2 + 4
    else:
        x_2 = 0
        x_3 = x_1 + w_1 + 4

    if old_max_lines > max_lines:
        [differ_labels.pop()[0].destroy() for _ in range(old_max_lines - max_lines)]
    elif max_lines > old_max_lines:
        [differ_labels.append([tk.Label(window), 0]) for _ in range(max_lines - old_max_lines)]
    dif_label_x = x_3 - 3
    for i in range(max_lines):
        y_pos = widget_y + (i * font_h) + 5
        differ_labels[i][0].config(bg=app_config['differ_colour'])
        # differ_labels[i][0].place(x=dif_label_x, y=y_pos, width=2, height=font_h - 6)
        differ_labels[i][1] = y_pos

    x_4 = x_3 + w_2 + 4
    w_4 = (font_w * comments_max_chars) + 6
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
        window.after(1, lambda: navigate_to(navigation))
    comments_x = 6
    comments_y = 35
    comments_w = (font_w * (10 + comments_max_chars)) + 6
    comments_h = (font_h * 25) + 4
    comments_win_w = comments_x + comments_w + 5
    comments_win_h = comments_y + comments_h + 5
    if comments_window:
        _, __, comments_win_x, comments_win_y = geometry(comments_window.geometry())
        comments_list_box.config(font=('Courier', main_font_size))
        comments_list_box.place(x=comments_x, y=comments_y, width=comments_w, heigh=comments_h)
        comments_window.geometry('{}x{}+{}+{}'.format(comments_win_w, comments_win_h, comments_win_x, comments_win_y))
    func_list_x = jumps_list_x = 6
    func_list_w = jumps_list_w = (font_w * (21 + comments_max_chars)) + 6
    func_list_h = jumps_list_h = (font_h * 14) + 4
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


dimension_window = font_size_label = line_amount_label = None
def change_win_dimensions():
    global dimension_window, font_size_label, line_amount_label
    if dimension_window:
        dimension_window.deiconify()
        dimension_window.focus_force()
        return
    dimension_window = tk.Tk()
    dimension_window.title('Change window dimensions')
    def scale_callback(amt, which):
        # new_size, new_lines = scale_size.get(), scale_lines.get()
        amt = int(amt)
        if which == 'size':
            set_widget_sizes(new_size=amt)
        elif which == 'lines':
            set_widget_sizes(new_max_lines=amt)
    scale_size = tk.Scale(dimension_window, from_=4, to=30, command=lambda amt: scale_callback(amt, 'size'))
    scale_lines = tk.Scale(dimension_window, from_=1, to=150, command=lambda amt: scale_callback(amt, 'lines'))
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
    font_size_label = tk.Label(dimension_window, text='Font size').pack(side=tk.LEFT)
    scale_size.pack(side=tk.LEFT)
    line_amount_label = tk.Label(dimension_window, text='Line amount').pack(side=tk.RIGHT)
    scale_lines.pack(side=tk.RIGHT)
    dimension_window.bind('<MouseWheel>', dimension_scroll_callback)
    dimension_window.bind('<Escape>', lambda e: dimension_window_equals_none())
    dimension_window.protocol('WM_DELETE_WINDOW', dimension_window_equals_none)
    dimension_window.focus_force()
    dimension_window.resizable(False, False)
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
    if manual_cic_win:
        manual_cic_win.deiconify()
        manual_cic_win.focus_force()
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
    manual_cic_win.resizable(False, False)
    manual_cic_win.mainloop()


def toggle_calc_crc():
    if not disassembler_loaded():
        return
    app_config['calc_crc'][disasm.hack_file_name] = not app_config['calc_crc'][disasm.hack_file_name]
    save_config()


changes_win = changes_list_box = None
changes_curselect = 0
def scour_changes():
    global changes_win, changes_list_box, changes_curselect
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
            i = disasm.region_align(i)
            i += disasm.game_offset
        address = extend_zeroes(hexi(i), 8)
        the_text = '{}: {}    {}'.format(address,hex_of,instruction)
        if key in disasm.comments:
            the_text += '  | {}'.format(disasm.comments[key])
        display_list.append(the_text)

    def changes_win_equals_none():
        global changes_win, changes_curselect
        changes_curselect = 0
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
            global changes_curselect
            curselect = changes_list_box.curselection()
            if not curselect:
                return
            changes_curselect = curselect[0]
            apply_hack_changes()
            apply_comment_changes()
            entry = changes_list_box.get(curselect[0])
            increment = 0 if not disasm.game_address_mode else -(disasm.game_offset >> 2)
            address = deci(entry[:8]) >> 2
            reset_target()
            navigate_to(address + increment, center=True, widget=None, region_treatment=True, region_override=True)

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
        changes_win.focus_force()
        change_colours()
        changes_win.resizable(False, False)
        changes_win.mainloop()



script_win = script_help_button = plus_button = out_file_button = view_test_button = save_template_button = \
    load_template_button = group_list_box = groups_label = groups_refresh_button = batch_label = script_label = \
    script_text_box = batch_text_box = None
def generate_script():
    global script_win, script_help_button, plus_button, out_file_button, view_test_button, save_template_button, \
        load_template_button, group_list_box, groups_label, groups_refresh_button, batch_label, script_label, \
        script_text_box, batch_text_box
    if not disassembler_loaded():
        return
    if script_win:
        script_win.deiconify()
        script_win.focus_force()
        return
    script_win = tk.Tk()
    script_win.title('Generate script')
    script_win.geometry('900x666+50+50')  # not intentional
    script_text_box = tk.Text(script_win, font=('Courier', 10))
    batch_text_box = tk.Text(script_win, font=('Courier', 10))

    def scriptify(text):
        def replace_tags(line, iter_text, note_text):
            try:
                if not len(iter_text) == 8:
                    raise Exception()
                # Raises exception if the first 8 chars cannot be converted from hex to int
                _ = deci(iter_text)
            except:
                msg = 'Unable to translate the address within the first 8 characters on this line: \n'\
                      '{}'.format(iter_text + note_text)
                simpledialog.messagebox._show('Error', msg, parent=script_win)
                raise
            if not note_text:
                note_text = '0x' + iter_text
            return line.replace('{{iter}}', iter_text).replace('{{note}}', note_text)

        split_text = text.split('\n')
        batch = batch_text_box.get('1.0', tk.END)
        if len(batch) == batch.count('\n'):
            simpledialog.messagebox._show('Error', 'You need items in the batch. Try grouping '
                                                   'some addresses by adding #groupname at the '
                                                   'end of their comments.',
                                          parent=script_win)
            return
        return_script = '\n'
        batch = batch.split('\n')
        header_items = []
        if '{{header}}' in text:
            if '{{endheader}}' not in text:
                simpledialog.messagebox._show('Error', 'You need to close {{header}} with {{endheader}}',
                                              parent=script_win)
                return
            in_header = False
            popped = 0
            iter_split_text = text.split('\n')
            for i in range(len(iter_split_text)):
                line = iter_split_text[i]
                popeye = split_text.pop(i - popped)
                popped += 1
                if line:
                    if line == '{{header}}':
                        in_header = True
                        continue
                    if in_header:
                        if line == '{{endheader}}':
                            break
                        else:
                            if ('{{iter}}' in line or '{{note}}' in line) and not '\t' in line:
                                header_items.append(popeye)
                            else:
                                return_script += line + '\n'
        if header_items:
            for script in header_items:
                for b in batch:
                    if not b:
                        continue
                    try:
                        replaced = replace_tags(script, b[:8], b[10:])
                    except:
                        return
                    if not replaced:
                        continue
                    return_script += replaced + '\n'
                return_script += '\n'
            return_script += '\n'

        for b in batch:
            if not b:
                continue
            for script in split_text:
                try:
                    replaced = replace_tags(script, b[:8], b[10:])
                except:
                    return
                if not replaced:
                    continue
                return_script += replaced + '\n'
            return_script += '\n'

        return return_script

    def out_file_callback():
        file_name = filedialog.asksaveasfilename(title='Select the directory and script name',
                                                 initialdir=app_config['script_output_dir'],
                                                 parent=script_win)
        if not file_name:
            return
        file_name = os.path.realpath(file_name)
        file_dir = file_name[:file_name.rfind('\\') + 1]
        app_config['script_output_dir'] = file_dir
        save_config()
        scriptified_text = scriptify(script_text_box.get('1.0', tk.END))
        if not scriptified_text:
            return
        with open(file_name, 'w') as script_file:
            script_file.write(scriptified_text)

    def help_button_callback():
        msg = '\n'.join([
            'You can create a script to apply the code you type in to every address in the batch. '
            'You can write the script in any language you wish to. ',
            'So you want to populate the batch with either the plus button or input your own addresses. '
            'You can "group" your comments by adding hashtags to the end. (eg. Some comment #example group)',
            'Basically, you only have to type out 1 instance of what you want to happen to each '
            'address in the batch, but replace a few pieces with {{}} tags.',
            '',
            'Tags:',
            '',
            '{{iter}} - Used to return the address iterating upon (first 8 chars) within the batch.',
            'Javascript example:',
            'events.onexec(0x{{iter}}, function() {',
            '  \tconsole.log(\'CPU is executing 0x{{iter}}\');',
            '}',
            '',
            '{{note}} - Can be used instead of {{iter}} for string operations.',
            'If no comment exists on that batch item (has less than 10 chars), it will translate to '
            '"0x{{iter}}". Just be careful which symbols you use in the comments, as they may cause '
            'errors in your script depending on the language',
            'Javascript example:',
            'events.onexec(0x{{iter}}, function() {',
            '  \tconsole.log(\'CPU is executing {{note}}\');',
            '}',
            '',
            '{{header}} - Can be used to mark header items, such as var declaration. Must be closed with '
            '{{endheader}}. You can also use {{iter}} or {{note}} in conjunction with header items. '
            'Any line of code inside the header which doesn\'t contain {{iter}} or {{note}} will not '
            'be copied for each batch item.',
            'Javascript example:',
            '{{header}}',
            'var count = new Object(); //No tags = only one instance generated.',
            'count[\'{{iter}}\'] = 0;',
            'var note{{iter}} = "{{note}}";',
            '{{endheader}}',
            'events.onexec(0x{{iter}}, function() {',
            '  \tconsole.log(\'CPU is executing \' + note{{iter}});',
            '  \tconsole.log(\'Count: \' + ++count[\'{{iter}}\']);',
            '}',
            '',
            'There are 2 example scripts inside your script templates folder. View them with "Load '
            'script template", then see what the output script will be with "Generate and view script".'
        ])
        simpledialog.messagebox._show('Script help', msg, parent=script_win)

    def plus_button_callback():
        curselect = group_list_box.curselection()
        if not curselect:
            return
        group_select = groups[group_list_box.get(curselect[0])]
        current_batch_text = batch_text_box.get('1.0', tk.END)
        if current_batch_text:
            if not current_batch_text.count('\n') == len(current_batch_text):
                if current_batch_text[-1] != '\n':
                    batch_text_box.insert(tk.END, '\n')
        for address in group_select:
            comment_key = str((deci(address) - disasm.game_offset) >> 2)
            address = disasm.region_align(deci(address), game_offset=True)
            batch_text_box.insert(tk.END, extend_zeroes(hexi(address), 8))
            if comment_key in disasm.comments:
                batch_text_box.insert(tk.END, ': ' + disasm.comments[comment_key])
            batch_text_box.insert(tk.END, '\n')

    def view_test_callback():
        scriptified = scriptify(script_text_box.get('1.0', tk.END))
        if not scriptified:
            return
        test_win = tk.Tk()
        test_win.title('Test script generation')
        test_win.geometry('1000x900+50+50')
        test_box = tk.Text(test_win)
        test_box.place(x=5, y=5, width=990, height=890)
        test_box.insert('1.0', scriptified)

    def save_template_callback():
        script = script_text_box.get('1.0', tk.END)
        template_file_path = filedialog.asksaveasfilename(title='Save script template',
                                                          initialdir=SCRIPTS_DIR,
                                                          parent=script_win)
        if not template_file_path:
            return
        template_file_path = os.path.realpath(template_file_path)
        with open(template_file_path, 'w') as template_file:
            template_file.write(script)

    def load_template_callback():
        template_file_path = filedialog.askopenfilename(title='Open script template',
                                                        initialdir=SCRIPTS_DIR,
                                                        parent=script_win)
        if not template_file_path:
            return
        template_file_path = os.path.realpath(template_file_path)
        with open(template_file_path, 'r') as template_file:
            script_text_box.delete('1.0', tk.END)
            script_text_box.insert('1.0', template_file.read())

    groups = {}
    def populate_list():
        backup_groups = groups.copy()
        for i in backup_groups:
            del groups[i]
        group_list_box.delete(0, tk.END)
        for key in disasm.comments:
            if '#' in disasm.comments[key]:
                place = disasm.comments[key].rfind('#')
                group = disasm.comments[key][place:]
                address = extend_zeroes(hexi((int(key) << 2) + disasm.game_offset), 8)
                if group in groups:
                    groups[group].append(address)
                else:
                    groups[group] = [address]

        [group_list_box.insert(tk.END, group) for group in groups]


    script_help_button = tk.Button(script_win, text='?', command=help_button_callback)
    plus_button = tk.Button(script_win, text='+', command=plus_button_callback)
    out_file_button = tk.Button(script_win, text='Generate and save as', command=out_file_callback)
    view_test_button = tk.Button(script_win, text='Generate and view script', command=view_test_callback)
    save_template_button = tk.Button(script_win, text='Save script template', command=save_template_callback)
    load_template_button = tk.Button(script_win, text='Load script template', command=load_template_callback)
    group_list_box = tk.Listbox(script_win)
    populate_list()

    groups_label = tk.Label(script_win, text='Groups')
    groups_label.place(x=5, y=8)
    groups_refresh_button = tk.Button(script_win, text='Refresh', command=populate_list)
    groups_refresh_button.place(x=60, y=5)
    batch_label = tk.Label(script_win, text='Batch')
    batch_label.place(x=191, y=8)
    script_label = tk.Label(script_win, text='Script')
    script_label.place(x=5, y=280-34)
    script_help_button.place(x=869, y=274-34, width=25, height=25)
    group_list_box.place(x=5, y=34, width=150, height=200)
    plus_button.place(x=160, y=34, width=25, height=200)
    batch_text_box.place(x=191, y=34, width=704, height=200)
    out_file_button.place(x=100, y=274-34)
    view_test_button.place(x=280, y=274-34)
    save_template_button.place(x=460, y=274-34)
    load_template_button.place(x=640, y=274-34)
    script_text_box.place(x=5, y=305-34, width=890, height=390)

    def script_win_equals_none():
        global script_win
        batch_text = batch_text_box.get('1.0', tk.END)
        if batch_text.count('\n') == len(batch_text):
            remember_batch = ''
        else:
            remember_batch = batch_text
            if remember_batch[-1] == '\n':
                remember_batch = remember_batch[:-1]
        script_text = script_text_box.get('1.0', tk.END)
        if script_text.count('\n') == len(script_text):
            remember_script = ''
        else:
            remember_script = script_text
            if remember_script[-1] == '\n':
                remember_script = remember_script[:-1]
        app_config['remember_script'][disasm.hack_file_name] = remember_script
        app_config['remember_batch'][disasm.hack_file_name] = remember_batch
        save_config()
        script_win.destroy()
        script_win = None

    if app_config['remember_script'][disasm.hack_file_name]:
        script_text_box.insert('1.0', app_config['remember_script'][disasm.hack_file_name])
    if app_config['remember_batch'][disasm.hack_file_name]:
        batch_text_box.insert('1.0', app_config['remember_batch'][disasm.hack_file_name])

    script_win.protocol('WM_DELETE_WINDOW', script_win_equals_none)
    change_colours()
    script_win.resizable(False, False)
    script_win.mainloop()


phrases_win = phrases_list_box = None
phrases_curselect = 0
def find_phrase():
    global phrases_win, phrases_list_box, phrases_curselect
    phrases = []
    while True:
        phrase = simpledialog.askstring('Find what'
                                        if not phrases else
                                        'Followed by',
                                        'Leave blank/cancel to begin search\n'
                                        'Input "-" to indicate this line does not matter\n'
                                        'Use "-*x" where x is any number to indicate x amount of lines which don\'t matter\n'
                                        'Use "-**" to indicate any amount of lines don\'t matter until we run into the next phrase, or leave the current function\n'
                                        'Use "*" as an AND phrase separator for this line\n'
                                        'Use "\\" as an OR phrase separator for this line\n'
                                        'ORs will be resolved before ANDs\n'
                                        if phrases else
                                        'Use "*" as an AND phrase separator for this line\n'
                                        'Use "\\" as an OR phrase separator for this line\n'
                                        'ORs will be resolved before ANDs\n')
        if not phrase:
            break
        phrases.append(phrase.upper())
    if not phrases:
        return
    checking = disasm.hack_file
    # ints = ints_of_4_byte_aligned_region(checking)
    display_list = []
    percent = len(checking) // 400
    found = []
    i = 0
    wildcards = 0
    function_end = -1
    len_flounder = 0

    while i < len(checking):
        j = i >> 2
        if not j % percent:
            status_text.set('Searching... ' + str(j // percent) + '%')
            status_bar.update()
        if wildcards:
            wildcards -= 1
            i += 4
            continue
        if j > function_end and function_end != -1:
            function_end = -1
            found[:] = []
            len_flounder = 0

        len_found = len(found) + len_flounder
        decoded = disasm.decode(int_of_4_byte_aligned_region(checking[i:i+4]), j)
        if function_end < 0:
            if len(phrases[len_found]) > 2:
                if phrases[len_found][:2] == '-*':
                    try:
                        wildcards = int(phrases[len_found][2:])
                        len_flounder += 1
                        i += 4
                        continue
                    except:
                        if phrases[len_found][2] == '*':
                            function_end = disasm.find_jumps(j, only_return_function_end=True)
                            len_found += 1
                            len_flounder += 1
                            if function_end < 0:
                                len_flounder = 0
                                len_found = 0
                                found[:] = []

        if phrases[len_found] == '-':
            found.append(j)
            len_found += 1
        elif not decoded:
            found[:] = []
            len_flounder = 0
            function_end = -1
        elif '*' in phrases[len_found]:
            inner_phrases = phrases[len_found].split('*')
            passed = 0
            for phrase in inner_phrases:
                if '\\' in phrase:
                    or_phrases = phrase.split('\\')
                    for l in or_phrases:
                        if l in decoded:
                            passed += 1
                            break
                else:
                    if phrase in decoded:
                        passed += 1
            if passed == len(inner_phrases):
                # if function_end < 0:
                found.append(j)
                len_found += 1
                function_end = -1
            elif function_end < 0:
                found[:] = []
                len_flounder = 0
        elif '\\' in phrases[len_found]:
            inner_phrases = phrases[len_found].split('\\')
            passed = False
            for l in inner_phrases:
                if l in decoded:
                    # if function_end < 0:
                    found.append(j)
                    function_end = -1
                    len_found += 1
                    passed = True
                    break
            if not passed and function_end < 0:
                found[:] = []
                len_flounder = 0
        elif phrases[len_found] in decoded:
            # if function_end:
            found.append(j)
            len_found += 1
            function_end = -1
        elif function_end < 0:
            found[:] = []
            len_flounder = 0
        # print(len(found))
        if len_found == len(phrases):
            for offset in found:
                decoded = disasm.decode(int_of_4_byte_aligned_region(checking[offset<<2:(offset<<2)+4]), offset)
                offset <<= 2
                if disasm.game_address_mode:
                    offset = disasm.region_align(offset) + disasm.game_offset
                display_list.append(extend_zeroes(hexi(offset), 8) + ': ' + decoded)
            if len(found) > 1:
                display_list.append('')
            found[:] = []
            len_flounder = 0
        i += 4

    def phrases_win_equals_none():
        global phrases_win, phrases_curselect
        phrases_curselect = 0
        phrases_win.destroy()
        phrases_win = None

    if phrases_win:
        phrases_win_equals_none()
    if display_list:
        phrases_win = tk.Tk()
        divide_by = (len(phrases) + 1) if len(phrases) > 1 else 1
        phrases_win.title('{} Results'.format(len(display_list) // divide_by))
        phrases_win.geometry('400x500+50+50')
        phrases_list_box = tk.Listbox(phrases_win, font=('Courier', 10))
        [phrases_list_box.insert(tk.END, i) for i in display_list]

        def list_callback():
            global phrases_curselect
            curselect = phrases_list_box.curselection()
            if not curselect:
                return
            phrases_curselect = curselect[0]
            entry = phrases_list_box.get(curselect[0])
            if not entry:
                return
            apply_hack_changes()
            apply_comment_changes()
            increment = 0 if not disasm.game_address_mode else -(disasm.game_offset >> 2)
            address = deci(entry[:8]) >> 2
            reset_target()
            navigate_to(address + increment, center=True, widget=None, region_treatment=True)

        phrases_list_box.bind('<<ListboxSelect>>', lambda _: list_callback())
        phrases_list_box.place(x=5, y=5, width=390, height=490)
        phrases_win.protocol('WM_DELETE_WINDOW', phrases_win_equals_none)
        phrases_win.bind('<Escape>', lambda _: phrases_win_equals_none())
        phrases_win.bind('<F1>', lambda e: view_comments())
        phrases_win.bind('<F3>', lambda e: toggle_base_file())
        phrases_win.bind('<F4>', lambda e: navigation_prompt(root=phrases_win))
        phrases_win.bind('<F5>', lambda e: toggle_address_mode())
        phrases_win.bind('<Control-s>', lambda e: save_changes_to_file())
        phrases_win.bind('<Control-S>', lambda e: save_changes_to_file())
        phrases_win.focus_force()
        change_colours()
        phrases_win.resizable(False, False)
        phrases_win.mainloop()
    else:
        simpledialog.messagebox._show('Oh no', 'No results for:\n\n' + '\n'.join(phrases))


mem_regions_win = id_count_unique_name = None
def set_memory_regions():
    global mem_regions_win, id_count_unique_name
    if not disassembler_loaded():
        return
    if mem_regions_win:
        mem_regions_win.deiconify()
        mem_regions_win.focus_force()
        return
    if not save_changes_to_file():
        simpledialog.messagebox._show('Alert', 'This feature requires that you save your changes beforehand. '
                                               'If you set the regions improperly, there is a possibility '
                                               'that it will corrupt the session and then you will be unable'
                                               ' to save thus you will lose your changes. You need to correct '
                                               'all of your errors to save.\n\n'
                                               'The application error which causes the session to corrupt is '
                                               'thrown when your region parameters cause a ROM region to fall '
                                               'under 0x00000000.')
        return
    mem_regions_win = tk.Tk()
    mem_regions_win.title('Set memory regions')
    widget_h = 22
    widget_w = 150
    sub_buttons = []
    mem_start_inputs = []
    mem_end_inputs = []
    mem_offset_inputs = []
    padding = 8
    row_1 = padding
    row_2 = row_1 + widget_w + padding
    row_3 = row_2 + widget_w + padding
    row_4 = row_3 + widget_w + padding
    id_count_unique_name = 0
    tk.Label(mem_regions_win, text='Virtual Address Start').place(x=row_1, y=padding)
    tk.Label(mem_regions_win, text='Region Length').place(x=row_2, y=padding)
    tk.Label(mem_regions_win, text='RAM minus ROM Offset').place(x=row_3, y=padding)

    y_offset = lambda amt_columns: 30 + padding + (amt_columns * (widget_h + padding))

    def resize_window():
        win_w = (widget_w * 3) + (padding*5) + widget_h
        win_h = y_offset(len(mem_start_inputs)) + 30
        mem_regions_win.geometry('{}x{}'.format(win_w, win_h))

    def add_field():
        global id_count_unique_name
        widget_y = y_offset(len(mem_start_inputs))

        mem_start_inputs.append(tk.Text(mem_regions_win, font=('Courier', 10)))
        mem_end_inputs.append(tk.Text(mem_regions_win, font=('Courier', 10)))
        mem_offset_inputs.append(tk.Text(mem_regions_win, font=('Courier', 10)))
        sub_buttons.append(tk.Button(mem_regions_win, text='-'))

        removing = [mem_start_inputs[-1], mem_end_inputs[-1], mem_offset_inputs[-1], sub_buttons[-1]]

        def sub_button_callback():
            for j, widget in enumerate(mem_start_inputs):
                if widget is removing[0]:
                    mem_start_inputs.pop(j).destroy()
                    mem_end_inputs.pop(j).destroy()
                    mem_offset_inputs.pop(j).destroy()
                    sub_buttons.pop(j).destroy()
                    while j < len(mem_start_inputs):
                        widget_y = y_offset(j)
                        mem_start_inputs[j].place(x=row_1, y=widget_y, width=widget_w, height=widget_h)
                        mem_end_inputs[j].place(x=row_2, y=widget_y, width=widget_w, height=widget_h)
                        mem_offset_inputs[j].place(x=row_3, y=widget_y, width=widget_w, height=widget_h)
                        sub_buttons[j].place(x=row_4, y=widget_y, width=widget_h, height=widget_h)
                        j += 1
                    add_button.place(x=row_4, y=y_offset(j), width=widget_h, height=widget_h)
                    resize_window()
                    break
        sub_buttons[-1].config(command=sub_button_callback)

        mem_start_inputs[-1].place(x=row_1, y=widget_y, width=widget_w, height=widget_h)
        mem_end_inputs[-1].place(x=row_2, y=widget_y, width=widget_w, height=widget_h)
        mem_offset_inputs[-1].place(x=row_3, y=widget_y, width=widget_w, height=widget_h)
        sub_buttons[-1].place(x=row_4, y=widget_y, width=widget_h, height=widget_h)

        add_button.place(x=row_4, y=y_offset(len(mem_start_inputs)), width=widget_h, height=widget_h)

        resize_window()

    def mem_regions_win_equals_none():
        global mem_regions_win
        del app_config['memory_regions'][disasm.hack_file_name]
        app_config['memory_regions'][disasm.hack_file_name] = []
        mem_regions = app_config['memory_regions'][disasm.hack_file_name]

        def get_text(widget):
            text = widget.get('1.0', tk.END)
            text = text.replace('\n', '')
            return text
        try:
            for i in range(len(mem_start_inputs)):
                start = get_text(mem_start_inputs[i])
                end = get_text(mem_end_inputs[i])
                offset = get_text(mem_offset_inputs[i])
                if start and end and offset:
                    mem_regions.append([
                        deci(start),
                        deci(end),
                        deci(offset)
                    ])
        except:
            del app_config['memory_regions'][disasm.hack_file_name]
            app_config['memory_regions'][disasm.hack_file_name] = []
            simpledialog.messagebox._show('Error', 'Could not translate all inputs to decimal values', parent=mem_regions_win)
            return
        disasm.memory_regions = app_config['memory_regions'][disasm.hack_file_name].copy()
        status_text.set('Saved memory region data.')
        status_bar.update()
        save_config()
        navigate_to(navigation)
        mem_regions_win.destroy()
        mem_regions_win = None

    add_button = tk.Button(mem_regions_win, text='+', command=add_field)
    add_button.place(x=row_4, y=y_offset(len(mem_start_inputs)), width=widget_h, height=widget_h)
    mem_regions = app_config['memory_regions'][disasm.hack_file_name]
    if not mem_regions:
        add_field()
    else:
        for i in mem_regions:
            add_field()
            mem_start_inputs[-1].insert('1.0', extend_zeroes(hexi(i[0]), 8))
            mem_end_inputs[-1].insert('1.0', extend_zeroes(hexi(i[1]), 8))
            mem_offset_inputs[-1].insert('1.0', extend_zeroes(hexi(i[2]), 8))
    resize_window()
    mem_regions_win.protocol('WM_DELETE_WINDOW', mem_regions_win_equals_none)
    mem_regions_win.focus_force()
    mem_regions_win.resizable(False, False)
    mem_regions_win.mainloop()


hex_win = hex_win_float_label = hex_win_hex_label = hex_win_float_tbox = hex_win_hex_tbox = hex_win_double_chkbx = None
def float_to_hex_converter():
    global hex_win, hex_win_float_label, hex_win_hex_label, hex_win_float_tbox, hex_win_hex_tbox, hex_win_double_chkbx
    if hex_win:
        hex_win.deiconify()
        hex_win.focus_force()
        return

    def hex_win_equals_none():
        global hex_win
        hex_win.destroy()
        hex_win = None

    def float_text_callback():
        current_text = hex_win_float_tbox.get('1.0', tk.END).replace('\n','')
        try:
            if check_var.get():
                output_text = double_to_hex(float(current_text))
            else:
                output_text = float_to_hex(float(current_text))
        except:
            return
        hex_win_hex_tbox.delete('1.0', tk.END)
        hex_win_hex_tbox.insert('1.0', output_text)

    def hex_text_callback():
        current_text = hex_win_hex_tbox.get('1.0', tk.END).replace('\n','').replace(' ','')
        try:
            if check_var.get():
                output_text = hex_to_double(current_text)
            else:
                output_text = hex_to_float(current_text)
        except:
            return
        hex_win_float_tbox.delete('1.0', tk.END)
        hex_win_float_tbox.insert('1.0', output_text)

    def check_command_callback():
        widget = hex_win.focus_get()
        if widget is hex_win_hex_tbox:
            hex_win.after(1, hex_text_callback)
        elif widget is hex_win_float_tbox:
            hex_win.after(1, float_text_callback)

    hex_win = tk.Tk()
    hex_win.title('Float <--> Hex')
    hex_win.geometry('433x58')

    check_var = tk.IntVar(hex_win)
    check_var.set(0)
    hex_win_float_label = tk.Label(hex_win, text='Float')
    hex_win_hex_label = tk.Label(hex_win, text='Hex')
    hex_win_double_chkbx = tk.Checkbutton(hex_win, text='Double', var=check_var, command=check_command_callback)

    hex_win_float_label.place(x=5, y=4)
    hex_win_hex_label.place(x=250, y=4)
    hex_win_double_chkbx.place(x=50, y=2)

    hex_win_float_tbox = tk.Text(hex_win, font=('Courier', 12))
    hex_win_hex_tbox = tk.Text(hex_win, font=('Courier', 12))

    hex_win_float_tbox.place(x=5, y=30, width=240, height=24)
    hex_win_hex_tbox.place(x=250, y=30, width=178, height=24)

    hex_win_float_tbox.bind('<Key>', lambda e: hex_win.after(1, lambda: float_text_callback()))
    hex_win_hex_tbox.bind('<Key>', lambda e: hex_win.after(1, lambda: hex_text_callback()))
    hex_win_float_tbox.bind('<Button-1>', lambda e: float_text_callback())
    hex_win_hex_tbox.bind('<Button-1>', lambda e: hex_text_callback())

    hex_win.protocol('WM_DELETE_WINDOW', hex_win_equals_none)
    change_colours()
    hex_win_float_tbox.focus_force()
    hex_win.resizable(False, False)
    hex_win.mainloop()


def generate_live_patch_script():
    out_script = ''
    i = 0x1000
    percent = disasm.file_length // 100
    while i < disasm.file_length:
        cont = True
        if not i % percent:
            status_text.set('Scanning changes: {}%'.format(i // percent))
            status_bar.update()
        for j in range(4):
            if disasm.base_file[i+j] != disasm.hack_file[i+j]:
                cont = False
                break
        if not cont:
            address = disasm.region_align(i) + disasm.game_offset
            val = int_of_4_byte_aligned_region(disasm.hack_file[i:i+4])
            address, val = extend_zeroes(hexi(address), 8), extend_zeroes(hexi(val), 8)
            out_script += '\n'
            if str(i >> 2) in disasm.comments:
                out_script += '//' + disasm.comments[str(i >> 2)] + '\n'
            out_script += 'mem.u32[0x{}] = 0x{};'.format(address, val)
            decoded = disasm.decode(deci(val), i >> 2)
            if not decoded:
                decoded = 'NOP'
            out_script += ' //{}\n'.format(decoded)
        i += 4
    if app_config['script_output_dir'] == working_dir:
        out_dir = filedialog.askdirectory(title='Target your scripts dir within PJ64d')
        if not out_dir:
            return
        out_dir = os.path.realpath(out_dir) + '\\'
        app_config['script_output_dir'] = out_dir
        save_config()
    else:
        out_dir = app_config['script_output_dir']
    file_path = out_dir + disasm.hack_file_name[:disasm.hack_file_name.rfind('.')] + ' patch.js'
    with open(file_path, 'w') as file:
        file.write(out_script)
    status_text.set('Wrote patch script. Use by starting script. It will stop automatically.')


def test():
    # max_size = 1
    # # min_size = 2
    # hex_list = []
    # i = 0x40
    # percent = disasm.file_length // 400
    # while i < disasm.file_length:
    #     if not (i >> 2) % percent:
    #         status_text.set('Populating hex_list: {}%'.format((i >> 2) // percent))
    #         status_bar.update()
    #     intie = int_of_4_byte_aligned_region(disasm.hack_file[i:i+4])
    #     hex_list.append(extend_zeroes(hexi(intie), 8))
    #     i += 4
    # i = max_size
    # percent = len(hex_list) // 100
    # patterns = {}
    # while i < len(hex_list):
    #     if not i % percent:
    #         status_text.set('Detecting patterns: {}%'.format(i // percent))
    #         status_bar.update()
    #     j = -1
    #     while j >= -max_size:
    #         this_pattern = ''.join(hex_list[i+j:i])
    #         if this_pattern not in patterns:
    #             patterns[this_pattern] = [i+j+0x10]
    #         else:
    #             patterns[this_pattern].append(i+j+0x10)
    #         j -= 1
    #     i += 1
    # mio0 = ''.join([extend_zeroes(hexi(ord(i)), 2) for i in 'MIO0'])
    # print(mio0)
    # [print(extend_zeroes(hexi(i << 2), 8)) for i in patterns[mio0]]
    # i = 0
    # percent = len(patterns) // 100
    # occurances = {}
    # for key in patterns:
    #     if not i & percent:
    #         status_text.set('Sorting highest occurances: {}%'.format(i // percent))
    #         status_bar.update()
    #     i += 1
    #     occ_key = str(len(patterns[key]))
    #     if occ_key not in occurances:
    #         occurances[occ_key] = [key]
    #     else:
    #         occurances[occ_key].append(key)

    # [print(i) for i in reversed(sorted([int(j) for j in occurances]))]

    layout_bits = 0x00114760
    high_bit = 1 << 31

    # Handles getting next bit from layout bits
    class layout_ticker():
        def __init__(self, layout_bits_start):
            self.current_bit = high_bit
            self.layout_bits_start = layout_bits_start
            self.offset = 0
            self.int_operating = 0
        def tick(self):
            if self.current_bit == high_bit:
                off = self.layout_bits_start + self.offset
                self.int_operating = int_of_4_byte_aligned_region(disasm.hack_file[off:off+4])
            is_compressed = not bool(self.int_operating & self.current_bit)
            if self.current_bit == 1:
                self.current_bit <<= 31
                self.offset += 4
            else:
                self.current_bit >>= 1
            return is_compressed

    layout = layout_ticker(layout_bits)
    decompressed = []
    decompressed_len = 0x35378
    compressed_offset = (0x19D4 + layout_bits) - 0x10
    uncompressed_offset = (0xAED0 + layout_bits) - 0x10
    while len(decompressed) < decompressed_len:
        is_compressed = layout.tick()
        if is_compressed:
            b1, b2 = disasm.hack_file[compressed_offset:compressed_offset + 2]
            length = ((b1 & 0b11110000) >> 4) + 3
            offset = ((b1 & 0b1111) << 8) + b2 + 1
            while length:
                decompressed.append(disasm.hack_file[uncompressed_offset - offset])
                offset -= 1
                length -= 1
            compressed_offset += 2
        else:
            decompressed.append(disasm.hack_file[uncompressed_offset])
            uncompressed_offset += 1


def optimise_function():
    # This is just in testing phase. Such complex, very confuse
    return
    if not disassembler_loaded():
        return
    m_index = (navigation + get_cursor(hack_file_text_box)[1]) - 1
    _, start, end = disasm.find_jumps(m_index, apply_offsets=False)
    if start < 0x40 or end >= disasm.file_length:
        status_text.set('Attempt to find function bounds failed')
        return
    func_ints = ints_of_4_byte_aligned_region(disasm.hack_file[start:end+4])
    func = [[disasm.decode(i, (start >> 2) + j), (start >> 2) + j] for j, i in enumerate(func_ints)]
    branches = [[i[0], i[1], deci(i[0][i[0].rfind(app_config['immediate_identifier']) + 1:])]
                for i in func if i[0][:i[0].find(' ')] in BRANCH_FUNCTIONS]
    func_dict = {}
    for i in func:
        func_dict[str(i[1] << 2)] = i[0]
    jumps = [i for i in func if i[0][:i[0].find(' ')] in JUMP_FUNCTIONS]
    prev_end_start = end
    prev_branch_point = 0
    branches_to = [i[2] for i in branches]
    slots_freed = 0
    moved = func[-1][0] == 'NOP'
    thingo = False
    # Tidy up the footer's branches
    for i, ii in enumerate(reversed(func[:-2])):
        inst, index = ii
        if inst[:inst.find(' ')] in BRANCH_FUNCTIONS:
            branch_to = deci(inst[inst.rfind(app_config['immediate_identifier']) + 1:])
            _end = prev_end_start if not prev_branch_point else prev_branch_point
            if branch_to == _end and 'BEQ R0, R0' in inst:
                func[-2 - i][0] = 'NOP'
                slots_freed += 1
                for j, b in enumerate(branches):
                    binst, bindex, b_to = b
                    if b_to == index << 2:
                        binst = binst[:-8]
                        binst += extend_zeroes(hexi(_end), 8)
                        f_index = bindex - (start >> 2)
                        func[f_index][0] = binst
                prev_branch_point = func[-2 - i][1] << 2
                thingo = False
            if thingo:
                break
        elif not moved and inst != 'NOP':
            func[-1][0] = inst
            func[-2 - i][0] = 'NOP'
            moved = True
        elif moved and inst != 'NOP' and 'LW RA' not in inst and 'ADDIU SP' not in inst and not thingo:
            thingo = True
        elif thingo:
            break
        if 'LW RA' in inst or 'ADDIU SP' in inst:
            prev_end_start = func[-2 - i][1] << 2

    def wipe_reg():
        if buffer[-1]:
            buffer.append([])

    def get_opcode(inst):
        if not ' ' in inst:
            return inst
        return inst[:inst.find(' ')]

    buffer = [[]]
    continuing = 0

    for j, i in enumerate(func):
        if continuing:
            continuing -= 1
            continue
        inst = i[0]
        word = get_opcode(inst)
        if word in ['JALR', 'JAL'] + BRANCH_FUNCTIONS:
            wipe_reg()
            continuing = 1
            continue
        if 'SP' in inst:
            wipe_reg()
            continue
        if i[1] in branches_to:
            wipe_reg()
        words = inst.replace(',', '').replace('(', '').replace(')', '').replace(app_config['immediate_identifier'], '').split(' ')
        buffer[-1].append([words, i[1]])


    load_instructions = ['LB','LBU','LD','LDL','LDR','LH','LHU','LL','LLD','LW','LWL','LWR','LWU','LWC1','LDC1']
    store_instructions = ['SB','SC','SCD','SD','SDL','SDR','SH','SW','SWL','SWR','SWC1','SDC1']

    new_func = [i[0] for i in func]

    def find_reg(words):
        asdf = []
        for i in scratch_registers:
            if i in words[1:]:
                asdf.append(i)
        return asdf

    scratch_registers = ['T' + str(i) for i in range(10)] + ['AT', 'A0', 'A1', 'A2', 'A3']

    for b in buffer:
        reg_usage_map = []
        all_reg_use = {}
        for r in scratch_registers:
            all_reg_use[r] = False
        for words, index in b:
            inst = words[0]
            if inst in load_instructions:
                ''
            elif inst in store_instructions:
                ''
            elif inst == 'LUI':
                ''



    '''
    
    group LUIs into immediate value used
    
    '''


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
nav_menu.add_separator()
nav_menu.add_command(label='Search assembly', command=find_phrase)
nav_menu.add_separator()
nav_menu.add_command(label='Scour hack for all changes', command=scour_changes)
menu_bar.add_cascade(label='Navigation', menu=nav_menu)

tools_menu = tk.Menu(menu_bar, tearoff=0)
tools_menu.add_command(label='Float <--> Hex', command=float_to_hex_converter)
tools_menu.add_command(label='Generate script for batch of addresses', command=generate_script)
# tools_menu.add_separator()
# tools_menu.add_command(label='Generate run-time patch script for PJ64D', command=generate_live_patch_script)
tools_menu.add_separator()
tools_menu.add_command(label='Re-map jumps', command=remap_jumps)
# tools_menu.add_separator()
# tools_menu.add_command(label='Test', command=test)
tools_menu.add_separator()
tools_menu.add_command(label='Bypass CRC', command=bypass_crc)
tools_menu.add_command(label='Manually set CIC chip', command=manual_cic)
tools_menu.add_checkbutton(label='Calculate checksum when saving', command=toggle_calc_crc, variable=calc_crc)
menu_bar.add_cascade(label='Tools', menu=tools_menu)

opts_menu = tk.Menu(menu_bar, tearoff=0)
opts_menu.add_command(label='Set scroll amount', command=set_scroll_amount)
opts_menu.add_command(label='Set immediate value identifier', command=change_immediate_id)
opts_menu.add_separator()
opts_menu.add_command(label='Set memory editor offset', command=set_mem_edit_offset)
opts_menu.add_command(label='Set memory regions', command=set_memory_regions)
opts_menu.add_separator()
opts_menu.add_command(label='Toggle "game entry point" mode (F5)', command=toggle_address_mode)
opts_menu.add_command(label='Toggle hex mode (F6)', command=toggle_hex_mode)
opts_menu.add_command(label='Toggle hex/bin space separation (F7)', command=toggle_hex_space)
opts_menu.add_command(label='Toggle bin mode (F8)', command=toggle_bin_mode)
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
window.bind('<F8>', lambda e: toggle_bin_mode())
window.bind('<Control-s>', lambda e: save_changes_to_file())
window.bind('<Control-S>', lambda e: save_changes_to_file())
window.bind('<MouseWheel>', scroll_callback)
hack_file_text_box.bind('<Control-g>', lambda e: find_jumps())
hack_file_text_box.bind('<Control-G>', lambda e: find_jumps())
hack_file_text_box.bind('<Control-f>', lambda e: follow_jump())
hack_file_text_box.bind('<Control-F>', lambda e: follow_jump())
# hack_file_text_box.bind('<Control-o>', lambda e: optimise_function())
# hack_file_text_box.bind('<Control-O>', lambda e: optimise_function())


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
            elif event.widget in [comments_text_box, address_text_box, base_file_text_box]:
                reset_target()
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
        if not clip_content:
            return
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
dif_label_x = 0

status_text = tk.StringVar()
status_text.set('Welcome!')
status_bar = tk.Label(window, relief=tk.SUNKEN, bd=3, textvariable=status_text, anchor=tk.W)

if app_config['status_bar']:
    status_bar.pack(side=tk.BOTTOM, fill=tk.X, padx=5, pady=3)

differ_labels = [[tk.Label(window), 0] for _ in range(max_lines)]

set_widget_sizes()
window.protocol('WM_DELETE_WINDOW', close_window)
change_colours()

if app_config['open_roms_automatically'] and app_config['previous_base_opened'] and app_config['previous_hack_opened']:
    if exists(app_config['previous_base_opened']) and exists(app_config['previous_hack_opened']):
        window.after(1, open_files)


window.resizable(False, False)
window.mainloop()

