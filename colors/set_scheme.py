#!/usr/bin/env nix-shell
#! nix-shell -i python3 -p python3

import json
import re
import sys

def update_colors(query, formatter, scheme, text):
    """ Updates configuration text with a new 16b color scheme

    Args:
        formatter (str -> str -> str): function that returns
            a line for defining variables in desired syntax
        scheme (dict): the colorscheme
        text (str): the body text to be updated

    Returns:
        (str)
    """

    for n,code in enumerate(scheme["color"]):
        name = "color"+str(n)
        search = query(name)
        new_line = formatter(name, code)
        text = re.sub(search, new_line, text, flags=re.MULTILINE)
    for name in ["background", "foreground"]:
        search = query(name)
        new_line = formatter(name, scheme[name])
        text = re.sub(search, new_line, text, flags=re.MULTILINE)
    return text

def update_file(updater, path):
    """ Applies `updater` to file at `path`, overwriting the file.

    Args:
        updater (str -> str): the function to apply
        path (str): the path of the file
    """
    with open(path, 'r') as f:
        text = f.read()
    new_text = updater(text)
    with open(path, 'w') as f:
        f.write(new_text)

def x_formatter(name, code):
    """ Formats a line of color scheme for .Xresources and similar config files.
    """
    return "*." + name + ": " + code
def x_query(name):
    """ Generates match string for .Xresources and similar config files
    """
    return r"^\*\."+name+"\: .*"

def generic_query(name):
    return "^" +name+ " +.*"

def kitty_formatter(name, code):
    """ Formats a line of color scheme for kitty and similar config files.
    """
    return name + "   " + code

def haskell_formatter(name, code):
    """ Formats a line of color scheme for haskell and similar languages.
    """
    return name + ' = "' + code + '"'

def update_conky(scheme, text):
    """ Conky has a unique colorscheme format. Replaces update_colors.
    Args:
        scheme (dict): the colorscheme
        text (str): the body of conky's config file

    Returns:
        (str)
    """
    text = conky_sub("default_color",scheme["color"][7],text) #light grey
    text = conky_sub("color1", scheme["color"][13],text) #pink
    text = conky_sub("color2", scheme["color"][4],text) #blue
    text = conky_sub("color3", scheme["color"][6],text) #cyan
    text = conky_sub("color4", scheme["color"][1],text) #red
    text = conky_sub("color5", scheme["color"][15],text) #white, not FG

    return text

def conky_sub(name, color, config):
    search   = "    "+name+" =.*"
    new_line = "    "+name+" = '"+color[1:]+"',"
    return re.sub(search, new_line, config)

def main():
    with open(sys.argv[1], 'r') as f:
        scheme = json.load(f)

    def standard_updater(query, formatter):
        return (lambda x : update_colors(query, formatter, scheme, x))

    update_file(
        standard_updater(
            generic_query,
            kitty_formatter),
        "kitty/kitty.conf")
    update_file(
        standard_updater(
            generic_query,
            haskell_formatter),
        "xmonad/xmonad.hs")
    update_file(
        standard_updater(
            x_query,
            x_formatter),
        "Xresources")
    update_file(
            (lambda x : update_conky(scheme, x)),
        "conky/conky.conf")


if __name__ == "__main__":
    main() 
