# ~/.bash_profile: executed by bash(1) for login shells.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/login.defs
#umask 022

# include .bashrc if it exists
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# .privatebashrc 负责一些不适合公开的设置，比如 Proxy 账号、密码等。
# 这个文件的属性应该设为 600，不进行版本控制（因为可能使用 Public Repos）
if [ -f ~/.privatebashrc ]; then
    . ~/.privatebashrc
fi

# set PATH so it includes user's private bin if it exists
if [ -d ~/bin ] ; then
    PATH=~/bin:"${PATH}"
fi

# /usr/local/bin:/usr/bin:/bin:/usr/bin/X11 也是必需的目录，
# 如果系统的初始化文件没有加到 PATH，这里要补上去

# 对于 root 组的用户，在 PATH 里增加几个管理用的目录
if [ `id -g` = "0" ]; then
    PATH=/usr/local/sbin:/usr/sbin:/sbin:$PATH
fi

export PATH
