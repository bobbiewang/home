# -*- coding: utf-8 -*-
# find the emacs home directory and save it
# in win registry.
# @author: wilbur <wilbur.ma@qq.com>
# @date: 2012.07.17

import winreg, os, platform

# emacs structure
structure = "bin/emacs.exe"
keyLoc = winreg.HKEY_LOCAL_MACHINE
keyPath = "SOFTWARE\GNU\Emacs"
keyPathAmd64 = "SOFTWARE\Wow6432Node\GNU\Emacs"

def findHomeDir():
    startDir = "."
    maxDepth = 10
    curDepth = 0
    homeDir = ""
    while curDepth < maxDepth:
        curDir = startDir + "/" + structure
        if os.path.exists(curDir):
            homeDir = os.path.abspath(startDir)
            break
        curDepth += 1
        if startDir == ".":
            startDir = ".."
        else:
            startDir += "/" + ".."

    return homeDir

def writeKey(homeDir):
    if (homeDir == "") or (not os.path.exists(homeDir)):
        print("Cannot find emacs home directory...")
    else:
        homekey = winreg.CreateKey(keyLoc, keyPath)
        winreg.SetValueEx(homekey, "HOME", 0, winreg.REG_SZ, homeDir)
        print("Emacs home is set to " + homeDir)
        winreg.CloseKey(homekey)
        if platform.machine() == "AMD64":
            homekeyAmd64 = winreg.CreateKey(keyLoc, keyPathAmd64)
            winreg.SetValueEx(homekeyAmd64, "HOME", 0, winreg.REG_SZ, homeDir)
            winreg.CloseKey(homekeyAmd64)

def main():
    emacsHome = findHomeDir()
    writeKey(emacsHome)

if __name__ == "__main__":
    main()
