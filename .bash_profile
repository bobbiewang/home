# ~/.bash_profile: executed by bash(1) for login shells.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# 缺省的 umask 在文件 /etc/login.defs 中设置
#umask 022

# ~/.bashrc-preprocess 负责和 site 相关的设置，在 ~/.bashrc 之前读取。
# 这些设置本来应该放在 /etc/profile 或者 /etc/bash.bashrc 中，但因为某
# 些系统没有这些文件的写权限，所以在 $HOME 下增加这个文件。
# 这个文件的属性应该设为 600，不进行版本控制
# ~/.bashrc-preprocess 文件应该设置如下环境变量：
#   - EMACS_SERVER_MODE，可选项有 daemon、screen、dtach 等
#   - Proxy 账号、密码
if [ -f ~/.bashrc-preprocess ]; then
    . ~/.bashrc-preprocess
else
    EMACS_SERVER_MODE="daemon"
fi

# .bashrc 要在 .privatebashrc 后加载，因为可能要用到前者的一些设置
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# ~/.bashrc-postprocess 负责和 site 相关的设置，在 ~/.bashrc 之后读取。
if [ -f ~/.bashrc-postprocess ]; then
    . ~/.bashrc-postprocess
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
