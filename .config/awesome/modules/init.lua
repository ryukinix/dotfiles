-- Module: initialization awesomeWM: error handling and applications

local awesome = awesome
local naughty = require("naughty")
local awful   = require("awful")

-- {{{ Error handling
if awesome.startup_errors then
   naughty.notify({ preset = naughty.config.presets.critical,
                    title = "Oops, there were errors during startup!",
                    text = awesome.startup_errors })
end

do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end
-- }}}

-- {{{ Autostart windowless processes
local function run_once(cmd_arr)
   for _, cmd in ipairs(cmd_arr) do
      findme = cmd
      firstspace = cmd:find(" ")
      if firstspace then
         findme = cmd:sub(0, firstspace-1)
      end
      awful.spawn.with_shell(string.format("pgrep -u $USER -x %s > /dev/null || (%s)", findme, cmd))
   end
end
run_once({"exec ~/.config/awesome/autostart/autorun.sh"})
