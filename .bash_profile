# ~/.bash_profile: executed by bash(1) for login shells.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# 缺省的 umask 在文件 /etc/login.defs 中设置
#umask 022

# 判断一些程序是否已安装，并设置相应的环境变量

HAVE_DTACH=`which dtach`
HAVE_SCREEN=`which screen`

export HAVE_DTACH HAVE_SCREEN

# .privatebashrc 负责一些不适合公开的设置，比如 Proxy 账号、密码等。
# 这个文件的属性应该设为 600，不进行版本控制（因为可能使用 Public Repos）
if [ -f ~/.privatebashrc ]; then
    . ~/.privatebashrc
fi

# .bashrc 要在 .privatebashrc 后加载，因为可能要用到前者的一些设置
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# 如果用户有自己的 ~/bin，加到 PATH 中
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
