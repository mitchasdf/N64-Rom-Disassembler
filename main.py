
import tkinter as tk
from tkinter import filedialog, simpledialog
import os
from binascii import unhexlify as unhex
from function_defs import *
from disassembler import Disassembler

CONFIG_FILE = 'rom disassembler.config'

# game_address_mode = False
disasm = None

# Setup app_config either fresh or from file
if os.path.exists(CONFIG_FILE):
    app_config = unpickle_data(CONFIG_FILE)
else:
    working_dir = os.path.dirname(os.path.realpath(__file__)) + '\\'
    app_config = {
        'previous_base_location': working_dir,
        'previous_hack_location': working_dir,
        'scroll_amount': 8,
        'immediate_identifier': '$',
        'game_address_mode': False
    }

'''
    A GUI with custom behaviour is required for user-friendliness.
    
    Displaying the whole file at once in a text box causes lag, so these text boxes will
      need to hold a small amount of data at a time in order to run smoothly.
      
    This can be done by maintain max_lines amount of lines at all times.
    
    Deviating from max_lines causes the list containing the data for the syntax checker
      to have a "shift". The syntax checker can't assume where or whether or not a shift
      has happened, so it needs the data to be preprocessed before the checker receives it.
    
    The only times the amount of lines will change is when:
      - User has pressed: Enter
                          BackSpace (if cursor.line != 1 and cursor.column == 0)
                          Delete or Ctrl+D (if cursor.line != max_lines and cursor.column == end_of_column)
      - 1 or more highlighted (or selected) newlines are replaced (or "typed/pasted over")
      - Data containing 1 or more newlines is pasted into the text box
      
    So this can be done by conditionally managing the data on each keypress.
      
'''

window = tk.Tk()
window.title('ROM Disassembler')
window.geometry('1337x810+550+50')

address_text_box = tk.Text(window, font = 'Courier')
base_file_text_box = tk.Text(window, font = 'Courier')
hack_file_text_box = tk.Text(window, font = 'Courier')
comments_text_box = tk.Text(window, font = 'Courier')

hack_file_text_box.tag_config('bad', background = 'red')
hack_file_text_box.tag_config('out_of_range', background = 'orange')

# [current_buffer_position, [[navigation, cursor_location, text_box_content], ...]]
hack_buffer = [0, []]
comments_buffer = [0, []]
buffer_max = 5000

# {'decimal_address': [error_code, text_attempted_to_encode], ...}
user_errors = {}

max_lines = 42
navigation = 0


def cursor_value(int1, int2):
    return '{}.{}'.format(int1, int2)


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
    if type_of(text) == 'str':
        text = text.split('\n')
    dot = cursor.find('.')
    line = int(cursor[:dot])
    column = int(cursor[dot + 1:])
    line = keep_within(line + line_amount, 1, len(text))
    line_length = len(text[line - 1])
    if type_of(column_amount) == 'int':
        column = keep_within(column + column_amount, 0, line_length)
    else:
        if column_amount == 'min':
            column = 0
        if column_amount == 'max':
            column = line_length
    return cursor_value(line, column)


def window_geo(window):
    # geometry format:
    # '{width}x{height}+{x_pos}+{y_pos}'
    #                  |---optional---|
    #                  |-when setting-|
    window_geo = window.geometry()
    mul_symbol = window_geo.find('x')
    plus_symbol_one = window_geo.find('+')
    plus_symbol_two = window_geo.find('+', plus_symbol_one + 1)

    window_w = int(window_geo[:mul_symbol])
    window_h = int(window_geo[mul_symbol + 1:plus_symbol_one])
    window_x = int(window_geo[plus_symbol_one + 1:plus_symbol_two])
    window_y = int(window_geo[plus_symbol_two + 1:])

    return window_w, window_h, window_x, window_y


# Is called pretty much after every time apply_hack_changes() is called
def highlight_errors():
    global max_lines, navigation
    hack_file_text_box.tag_remove('bad', '1.0', tk.END)
    hack_file_text_box.tag_remove('out_of_range', '1.0', tk.END)
    print(user_errors)
    for i in range(max_lines):
        navi = i + navigation
        key = '{}'.format(navi)
        if key in user_errors.keys():
            line_start = cursor_value(i + 1, 0)
            line_end = cursor_value(i + 2, 0)
            err_code = user_errors[key][0]
            # Red highlight for bad, orange highlight for out_of_range (tags set at end of script, before TKinter's window.mainloop())
            tag = 'bad' if err_code > -3 else 'out_of_range'
            hack_file_text_box.tag_add(tag, line_start, line_end)


# The hacked text box syntax checker, change applier and Disassembler.comments accumulator
def apply_hack_changes(ignore_slot = None):
    current_text = hack_file_text_box.get('1.0', tk.END)[:-1].upper()
    split_text = current_text.split('\n')
    for i in range(len(split_text)):
        navi = navigation + i
        if i == ignore_slot:
            continue
        is_hex_part = navi < 16
        string_key = '{}'.format(navi)
        if is_hex_part:
            split_text[i] = split_text[i].replace(' ', '')

        elif not split_text[i]:
            disasm.split_and_store_bytes(0, navi)
            hack_file_text_box.insert(cursor_value(i + 1, 0), 'NOP')

        elif split_text[i] != 'UNKNOWN/NOT AN INSTRUCTION':
            encoded_int = disasm.encode(split_text[i], navi)
            if encoded_int >= 0:
                disasm.split_and_store_bytes(encoded_int, navi)
            else:
                user_errors[string_key] = [encoded_int, split_text[i]]


def apply_comment_changes(ignore_slot = None):
    current_text = comments_text_box.get('1.0', tk.END)[:-1]
    split_text = current_text.split('\n')
    for i in range(len(split_text)):
        navi = navigation + i
        if not split_text[i]:
            continue
        string_key = '{}'.format(navi)
        disasm.comments[string_key] = split_text[i]


# Custom keyboard events and textbox behaviour upon any keypress in text boxes
# It seems extensive due to all the black magic I needed to inject
clipboard = ''
def keyboard_events(handle, max_char, event, buffer = None, hack_function = False):
    global navigation, max_lines, buffer_max, clipboard
    if not disasm:
        return
    after_delay = 2
    joined_text = handle.get('1.0', tk.END)
    if joined_text.count('\n') == max_lines:
        joined_text = joined_text[:-1]
    split_text = joined_text.split('\n')

    cursor, line, column = get_cursor(handle)

    ctrl_held = bool(event.state & 4)
    ctrl_d = ctrl_held and event.keysym == 'd'
    has_char = bool(event.char) and event.keysym != 'Escape' and not ctrl_held

    is_undoing = buffer and ctrl_held and event.keysym == 'comma'
    is_redoing = buffer and ctrl_held and event.keysym == 'period'
    is_cutting = buffer and ctrl_held and event.keysym == 'x'
    is_pasting = buffer and ctrl_held and event.keysym == 'v'
    is_copying = buffer and ctrl_held and event.keysym == 'c'
    is_saving = ctrl_held and event.keysym == 's'
    is_deleting = ctrl_d or event.keysym == 'Delete'
    is_backspacing = event.keysym == 'BackSpace'
    is_returning = event.keysym == 'Return'

    selection_removable = has_char or is_pasting or is_cutting or is_deleting

    not_arrows = event.keysym not in ['Left', 'Up', 'Right', 'Down']
    vert_arrows = event.keysym in ['Up', 'Down']

    apply_function = apply_hack_changes if hack_function else apply_comment_changes

    # Cause each modification of text box to snap-shot data in order to undo/redo
    if buffer and not (is_undoing or is_redoing) and has_char and not_arrows:
        # buffer[0] is the current buffer frame
        # buffer[1] is an array containing the buffer frames
        # buffer frames are an array of [current_navigation, cursor_location, text_box_content]

        buffer_length = len(buffer[1])
        distance_from_end = (buffer_length - 1) - buffer[0]
        # This condition means the user is not currently at the end of the buffer (they have done some undo's)
        # so delete all buffer frames following the current one so that the current buffer frame is at the top
        if distance_from_end and buffer_length:
            buffer[1] = buffer[1][:-distance_from_end]
            buffer_length -= distance_from_end
        buffer[1].append([navigation, cursor, joined_text])
        buffer[0] += 1

        # Start shifting buffer frames down and out of the buffer as the limit as been reached
        # Added diff slice in case buffer ever overflows
        diff = buffer_max - buffer[0]
        if diff < 0:
            buffer[0] -= diff
            buffer[1] = buffer[1][diff:]

    # Undoing and Redoing code
    if is_undoing or is_redoing:
        if buffer[0] == len(buffer[1]) - 1 and is_undoing:
            part = buffer[1][buffer[0]]
            if part[0] != navigation or part[2] != joined_text:
                buffer[1].append([navigation, cursor, joined_text])
                buffer[0] += 1

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
            handle.delete('1.0', tk.END)
            handle.insert('1.0', buffer[1][place][2])
            handle.mark_set(tk.INSERT, cursor)
            apply_hack_changes()
            apply_comment_changes()
            highlight_errors()
        return

    # Copy/Paste and selection handling
    # Get highlighted text boundaries and cause the bounding line's whole columns to be "selected"
    try:
        selection_start, sel_start_line, sel_start_column = get_cursor(handle, tk.SEL_FIRST)
        selection_end, sel_end_line, sel_end_column = get_cursor(handle, tk.SEL_LAST)
        if sel_start_line != sel_end_line:
            selection_start = modify_cursor(selection_start, 0, 'min', split_text)
            selection_end = modify_cursor(selection_end, 0, 'max', split_text)
            # vars not in use at the moment
            # sel_start_column = 0
            # sel_end_column = len(split_text[sel_end_line - 1])
    except:
        selection_start, sel_start_line, sel_start_column = '1.0', 0, 0
        selection_end, sel_end_line, sel_end_column = '1.0', 0, 0

    has_selection = selection_start != selection_end
    selection_lines = sel_end_line - sel_start_line
    selection_function = has_selection and (selection_removable or is_copying)
    standard_key = not is_backspacing and not is_returning and has_char
    lower_outer_bound_selection_char = handle.get(modify_cursor(selection_start, 0, -1, split_text))
    upper_outer_bound_selection_char = handle.get(selection_end)
    paste_text = ''
    lines_diff = 0

    # Because using mark_set() on SEL_FIRST or SEL_LAST seems to corrupt the widgets beyond repair at a windows level,
    # A work around with a custom clipboard is required in order for the code to be able to serve it's intended purpose
    if selection_function:
        selection_text = handle.get(selection_start, selection_end)

        if is_copying or is_cutting:
            clipboard = selection_text
            # So that when user pastes, there aren't duplicates from both clipboards
            window.after(after_delay - 1, window.clipboard_clear)

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
                    min_del = modify_cursor(cursor, 0, 'min', split_text)
                    max_del = modify_cursor(cursor, -lines_diff, 'max', split_text)
                    handle.delete(min_del, max_del)
                    lines_diff = 0
            paste_text = winnie_clip

    # Either clear lines which would be excess lines after the paste
    # Or add new lines to fill in what would be the gaps after the paste
    insertion_place = selection_start if has_selection else cursor
    if lines_diff > 0:
        handle.insert(insertion_place, '\n' * lines_diff)
    elif lines_diff < 0:
        handle.delete(insertion_place, modify_cursor(insertion_place, lines_diff * -1, 'max', split_text))

    if is_pasting or is_cutting:
        def move_next(handle):
            move_amount = 1 if is_pasting else 0
            handle.mark_set(tk.INSERT, modify_cursor(handle.index(tk.INSERT), move_amount, 'max', handle.get('1.0', tk.END)[:-1]))
        handle.insert(insertion_place, paste_text)
        window.after(after_delay - 1, lambda: (apply_hack_changes(),
                                               apply_comment_changes(),
                                               move_next(handle),
                                               navigate_to(navigation)))
    # Copy/Paste end

    # Easier than recalculating for each condition in the copy/paste section
    cursor, line, column = get_cursor(handle)
    joined_text = handle.get('1.0', tk.END)[:-1]
    split_text = joined_text.split('\n')

    nl_at_cursor = handle.get(cursor) == '\n'
    # Make any key delete the final character of the line if word is about to wrap onto next line
    # Also validate all code except line currently editing
    if standard_key:
        apply_function(ignore_slot = line - 1)
        line_chars = len(split_text[line - 1])
        if line_chars > max_char - 1:
            handle.delete(cursor_value(line, max_char - 1), cursor_value(line, max_char))

    # Make delete do nothing if cursor precedes a new line
    # Make backspace act as left arrow if cursor at column 0 then validate code (ignoring the line if cursor not at column 0)
    elif (is_backspacing and (column == 0 and line > 1)) or (is_deleting and nl_at_cursor):
        apply_function((line - 1) if not sel_start_line else None)
        handle.insert(cursor,'\n')
        handle.mark_set(tk.INSERT, cursor)

    # Make return send the cursor to the end of the next line and validate code
    elif is_returning:
        if line == max_lines:
            handle.mark_set(tk.INSERT, modify_cursor(cursor, -1, 'max', split_text))

        # if line == max_lines:
        #     seg_1 = line - 1
        #     seg_2 = len(split_text[line - 2])
        # else:
        #     seg_1 = line
        #     seg_2 = len(split_text[line - 1])
        # cursor = cursor_value(seg_1, seg_2)
        # new_cursor = cursor_value(seg_1 + 1, len(split_text[seg_1]))
        # handle.mark_set(tk.INSERT, cursor)
        # handle.delete(cursor, cursor_value(line + 1, 0))
        # # The delay is necessary to stop the syntax checker from noting every subsequent line from the cursor to be "changed" due to return's newline
        # window.after(after_delay - 1, lambda: (apply_function(), handle.mark_set(tk.INSERT, new_cursor)))

    # Prevent delete or backspace from modifying textbox any further than the bounds of the selected text (if selected text is only on one line)
    if has_selection and not selection_lines and (is_deleting or is_backspacing):
        replace_char = lower_outer_bound_selection_char if is_backspacing else upper_outer_bound_selection_char
        handle.insert(selection_start, replace_char)
        if is_deleting:
            window.after(after_delay - 1, lambda: handle.mark_set(tk.INSERT, selection_start))

    if vert_arrows:
        apply_function()

    if is_saving:
        save_changes_to_file()


    # Adding the delay fixes a problem where Enter would cause the syntax highlighting to drag along to the new line created when Enter fires after this function
    window.after(after_delay, highlight_errors)


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


def navigate_to(index):
    global navigation, change_rom_name_button, disasm
    if not disasm:
        return

    if change_rom_name_button:
        change_rom_name_button.destroy()
        change_rom_name_button = None

    # Correct the navigation if traveling out of bounds, also calculate limits for file samples to display
    amount_words = disasm.file_length // 4
    navigation = index if index + max_lines < amount_words else amount_words - max_lines
    if navigation < 0:
        navigation = 0
    limit = navigation + max_lines if navigation + max_lines < amount_words else amount_words
    lines = limit - navigation

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

    # Calculate what addresses to display in the address box, and disassemble ints into instructions, display header section as raw hex
    address_range = [extend_zeroes(hexi((i * 4) + (disasm.game_offset
                                                   if disasm.game_address_mode else 0)), 8) for i in range(navigation, limit)]
    base_disassembled = [disasm.decode(ints_in_base_sample[i], navigation + i) if navigation + i > 15 else \
                         hex_space(extend_zeroes(hexi(ints_in_base_sample[i]), 8)) \
                         for i in range(len(ints_in_base_sample))]
    hack_disassembled = [disasm.decode(ints_in_hack_sample[i], navigation + i) if navigation + i > 15 else \
                         hex_space(extend_zeroes(hexi(ints_in_hack_sample[i]), 8)) \
                         for i in range(len(ints_in_hack_sample))]

    # Display floating Rom Name Change button
    if disasm.header_items['Rom Name'][0] // 4 in range(navigation, limit):
        change_rom_name_button = tk.Button(window, text = 'Change', command = change_rom_name)
        y_offset = ((disasm.header_items['Rom Name'][0] // 4) - navigation) * 18
        change_rom_name_button.place(x = 830, y = 46 + y_offset, height = 20)

    # Update all 4 text boxes
    def update_text_box(handle, text):
        cursor, line, column = get_cursor(handle)
        handle.delete('1.0', tk.END)
        handle.insert('1.0', text)
        handle.mark_set(tk.INSERT, modify_cursor(cursor, 0, 'max', text))

    update_text_box(address_text_box, '\n'.join(address_range))
    update_text_box(base_file_text_box, '\n'.join(base_disassembled))
    update_text_box(hack_file_text_box, '\n'.join(hack_disassembled))
    update_text_box(comments_text_box, '\n'.join(sample_comments))


def navigation_prompt():
    if not disasm:
        return
    address = simpledialog.askstring('Navigate to address', '')
    try:
        address = deci(address) // 4
    except:
        return
    apply_hack_changes()
    apply_comment_changes()
    navigate_to(address)
    highlight_errors()


def scroll_callback(event):
    global navigation, disasm
    if not disasm:
        return
    apply_hack_changes()
    apply_comment_changes()
    direction = -app_config['scroll_amount'] if event.delta > 0 else app_config['scroll_amount']
    navigate_to(navigation + direction)
    highlight_errors()


def save_changes_to_file():
    global max_lines, disasm
    if not disasm:
        return

    apply_hack_changes()
    apply_comment_changes()

    # Needs to be copied: cannot change dict keys while iterating
    # hacked_keys = list(hack_changes.keys())
    error_keys = user_errors.keys()

    # Do not save changes if there is any encoding or re-decoding issue with any of the user's code
    for key in user_errors:
        i = int(key)
        navigate_to(i - (max_lines // 2))
        highlight_errors()
        return

    # Change the file saved on the RAM
    # for key in hacked_keys:
    #     i = int(key)
    #     if i > 15:
    #         code_bytes = disasm.encode(hack_changes[key], i).to_bytes(4, byteorder = 'big', signed = False)
    #     else:
    #         code_bytes = bytes.fromhex(hack_changes[key])
    #     for j in range(4):
    #         i_j = (i << 2) + j
    #         disasm.hack_file[i_j] = code_bytes[j]
    #     del hack_changes[key]

    with open(disasm.comments_file, 'w') as file:
        file.write(dict_to_string(disasm.comments))

    with open(disasm.hack_folder + disasm.hack_file_name, 'wb') as file:
        file.write(disasm.hack_file)


def close_window(side = 'right'):
    global disasm
    if not disasm:
        window.destroy()
        return

    close_win_width = 270
    close_win_height = 45
    close_win_y_offset = 130

    win_w, win_h, win_x, win_y = window_geo(window)

    placement_x = ((win_w if side == 'right' else close_win_width)  + win_x) - close_win_width
    placement_y = (close_win_y_offset + win_y) - close_win_height

    close_win_geo = '{}x{}+{}+{}'.format(close_win_width, close_win_height, placement_x, placement_y)

    close_win = tk.Tk()
    close_win.geometry(close_win_geo)
    close_win.title('Exit')
    label = tk.Label(close_win, text = 'Save work?').place(x = 150, y = 12)

    yes_button = tk.Button(close_win, text='Yes',command = lambda:\
        (save_changes_to_file(), window.destroy(), close_win.destroy()))
    no_button = tk.Button(close_win, text='No',command = lambda:\
        (window.destroy(), close_win.destroy()))

    yes_button.place(x=10, y=10, width=50)
    no_button.place(x=75, y=10, width=50)

    def cancel_close_win():
        global closing
        closing = False
        close_win.destroy()

    close_win.protocol('WM_DELETE_WINDOW', cancel_close_win)
    close_win.bind('<FocusOut>', lambda _: close_win.destroy())
    close_win.focus_force()
    close_win.mainloop()


def open_files(mode = ''):
    global disasm

    if disasm:
        # todo: handle if disasm already exists
        print('todo: handle if disasm already exists')
        return

    # Set data for rest of this function
    if mode == 'new':
        base_title = 'Select the base (original un-edited) rom (do not worry about backups - file is never modified)'
        hack_title = 'Choose location and name for the new hacked rom'
        hack_dialog_function = filedialog.asksaveasfilename
    else:
        base_title = 'Select the base rom'
        hack_title = 'Select the hacked rom'
        hack_dialog_function = filedialog.askopenfilename

    # Obtain file locations from user input
    base_file_path = filedialog.askopenfilename(initialdir = app_config['previous_base_location'], title = base_title)
    if not base_file_path:
        return
    base_file_path = os.path.realpath(base_file_path)
    base_dir = base_file_path[:base_file_path.rfind('\\') + 1]

    hack_dir = base_dir if mode == 'new' else app_config['previous_hack_location']
    hack_file_path = hack_dialog_function(initialdir = hack_dir, title = hack_title)
    if not hack_file_path:
        return
    hack_file_path = os.path.realpath(hack_file_path)
    hack_dir = hack_file_path[:hack_file_path.rfind('\\') + 1]

    if mode == 'new':
        if os.path.exists(hack_file_path):
            # todo: error file already exists
            return
        else:
            with open(base_file_path, 'rb') as base_file:
                with open(hack_file_path, 'wb') as hack_file:
                    hack_file.write(base_file.read())

    # Remember dirs for next browse
    app_config['previous_base_location'] = base_dir
    app_config['previous_hack_location'] = hack_dir
    pickle_data(app_config, CONFIG_FILE)

    # Initialise disassembler with paths to the 2 files, apply saved settings from app_config
    disasm = Disassembler(base_file_path, hack_file_path)
    disasm.game_address_mode = app_config['game_address_mode']
    disasm.immediate_identifier = app_config['immediate_identifier']

    # Navigate user to first line of code, start the undo buffer frame with the current data on screen
    navigate_to(0)
    hack_buffer[1].append([navigation, '1.0', hack_file_text_box.get('1.0', tk.END)[:-1]])
    comments_buffer[1].append([navigation, '1.0', comments_text_box.get('1.0', tk.END)[:-1]])


def toggle_address_mode():
    apply_hack_changes()
    apply_comment_changes()
    toggle_to = not app_config['game_address_mode']
    app_config['game_address_mode'] = toggle_to
    if disasm:
        disasm.game_address_mode = toggle_to
    navigate_to(navigation)


def change_immediate_id():
    symbol = simpledialog.askstring('Set immediate identifier symbol', 'Must be one of < > : ; \' " | { } = + - _ * & ^ % $ # . @ ! ` ~ / ? \\')
    if symbol and symbol[:1] in ['<', '>', ':', ';', '\'', '"', '|', '{', '}', '[', ']',
                                 '=', '+', '-', '_', '*', '&', '^', '%', '$', '#', '.',
                                 '@', '!', '`', '~', '/', '?', '\\']:
        hack_text = hack_file_text_box.get('1.0', tk.END)[:-1]
        hack_text.replace(app_config['immediate_identifier'], symbol[:1])
        hack_file_text_box.delete('1.0', tk.END)
        hack_file_text_box.insert('1.0', hack_text)
        for key in user_errors.keys():
            user_errors[key] = user_errors[key].replace(app_config['immediate_identifier'], symbol[:1])
        app_config['immediate_identifier'] = symbol[:1]
        if disasm:
            disasm.immediate_identifier = symbol[:1]
        pickle_data(app_config, CONFIG_FILE)


def set_scroll_amount():
    amount = simpledialog.askinteger('Set scroll amount', 'Current: {}'.format(app_config['scroll_amount']))
    if amount:
        app_config['scroll_amount'] = amount
        pickle_data(app_config, CONFIG_FILE)


menu_bar = tk.Menu(window)

file_menu = tk.Menu(menu_bar, tearoff=0)
file_menu.add_command(label='Start new hacked rom', command=lambda: open_files('new'))
file_menu.add_command(label='Open existing hacked rom', command=lambda: open_files('existing'))
file_menu.add_separator()
file_menu.add_command(label='Exit', command=lambda: close_window('left'))
menu_bar.add_cascade(label='File', menu=file_menu)

tool_menu = tk.Menu(menu_bar, tearoff=0) # todo
tool_menu.add_command(label='Navigate (F4)', command=navigation_prompt)
# tool_menu.add_command(label = 'Search (Ctrl+F)')
menu_bar.add_cascade(label='Tools', menu=tool_menu)

opts_menu = tk.Menu(menu_bar, tearoff=0)
opts_menu.add_command(label='Toggle "game entry point" mode (F5)', command=toggle_address_mode)
opts_menu.add_command(label='Change immediate value identifier', command=change_immediate_id)
opts_menu.add_command(label='Set scroll amount', command=set_scroll_amount)
menu_bar.add_cascade(label='Options', menu=opts_menu)

help_menu = tk.Menu(menu_bar,tearoff=0) # todo
help_menu.add_command(label='Help')
help_menu.add_command(label='About')
menu_bar.add_cascade(label='Help', menu=help_menu)

window.config(menu=menu_bar)

window.bind('<F4>', lambda e: navigation_prompt())
window.bind('<F5>', lambda e: toggle_address_mode())
window.bind('<MouseWheel>', scroll_callback)
window.bind('<Button-1>', lambda e: (apply_hack_changes(), apply_comment_changes(), highlight_errors()) \
                                     if disasm else None)

address_text_box.place(x=6, y=45, width=85, height=760)
base_file_text_box.place(x=95, y=45, width=315, height=760)
hack_file_text_box.place(x=414, y=45, width=315, height=760)
comments_text_box.place(x=733, y=45, width=597, height=760)


window.protocol('WM_DELETE_WINDOW', close_window)
window.mainloop()

