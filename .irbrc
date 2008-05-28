# -*- ruby -*-

require 'rubygems'

# 支持用 Tab 完成输入
require 'irb/completion'
IRB.conf[:USE_READLINE] = true

# 设置保存历史记录
require 'irb/ext/save-history'
IRB.conf[:SAVE_HISTORY] = 1000
IRB.conf[:EVAL_HISTORY] = 100

# 创建提示符
IRB.conf[:PROMPT][:CUSTOM] = { 
    :PROMPT_N => ">> ",
    :PROMPT_I => ">> ",
    :PROMPT_S => nil,
    :PROMPT_C => " > ",
    :RETURN => "=> %s\n"
}

# 选择一种提示符
IRB.conf[:PROMPT_MODE] = :CUSTOM

# 复位 irb，可以接收一个 block 作为参数
def reset_irb
  at_exit { exec($0) }
  at_exit { yield if block_given? }

  # 把当前的历史命令写到 .irb_history
  if num = IRB.conf[:SAVE_HISTORY] and (num = num.to_i) > 0
    if hf = IRB.conf[:HISTORY_FILE]
      file = File.expand_path(hf)
    end
    file = IRB.rc_file("_history") unless file
    open(file, 'w') do |file|
      hist = IRB::HistorySavingAbility::HISTORY.to_a
      file.puts(hist[-num..-1] || hist)
    end
  end

  # 结束当前 IRB 进程（由 at_exit 接管）
  throw :IRB_EXIT, 0
end

# 清屏
def clear
  eval "def clear; print #{ `clear`.inspect} end"
  clear
end
private :clear
