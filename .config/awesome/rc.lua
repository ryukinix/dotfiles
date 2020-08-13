--[[
   Awesome WM configuration template
   github.com/copycat-killer
   Which was stole by @lubien initially
   And stole from @lubien by @ryukinix (the owner now)
   Nested steals.
--]]

-- {{{ Required libraries
local awesome, client, mouse, screen, tag = awesome, client, mouse, screen, tag
local ipairs, string, os, table, tostring, tonumber, type = ipairs, string, os, table, tostring, tonumber, type

local gears         = require("gears")
local awful         = require("awful")
                      require("awful.autofocus")
local wibox         = require("wibox")
local beautiful     = require("beautiful")
local naughty       = require("naughty")
local lain          = require("lain")
local menubar       = require("menubar")
local freedesktop   = require("freedesktop")
local hotkeys_popup = require("awful.hotkeys_popup").widget
local cyclefocus    = require('cyclefocus')
-- }}}

-- Manoel here
-- limit icon size of notifications
naughty.config.defaults['icon_size'] = 50

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
-- manoel stuff here, fix of the fix from fixed fix
-- I need that shit done here, sorry
-- local posix = require("posix")
-- posix.setenv("SSH_AUTH_SOCK", "/run/user/1000/keyring/ssh")


run_once({"exec ~/.config/awesome/autorun.sh"})
-- entries must be comma-separated
-- }}}

-- {{{ Variable definitions
local chosen_theme = "multicolor"
local modkey       = "Mod4"
local altkey       = "Mod1"
local terminal     = "xfce4-terminal"
local alt_terminal = "alacritty"
local editor       = "emacsclient"
local gui_editor   = "/home/lerax/.local/bin/gemacs"
local browser      = "exo-open --launch WebBrowser"

awful.util.terminal = terminal
awful.util.tagnames = { "main", "dev", "tmp" }
awful.layout.layouts = {
   awful.layout.suit.floating,
   awful.layout.suit.tile,
   awful.layout.suit.floating,
   awful.layout.suit.tile.left,
   awful.layout.suit.tile.bottom,
   awful.layout.suit.tile.top,
   awful.layout.suit.max,
   awful.layout.suit.max.fullscreen,
   awful.layout.suit.magnifier,
   -- awful.layout.suit.fair,
   -- awful.layout.suit.fair.horizontal,
   --awful.layout.suit.spiral,
   --awful.layout.suit.spiral.dwindle,
   --awful.layout.suit.corner.nw,
   --awful.layout.suit.corner.ne,
   --awful.layout.suit.corner.sw,
   --awful.layout.suit.corner.se,
   --lain.layout.cascade,
   --lain.layout.cascade.tile,
   -- lain.layout.centerwork,
   -- lain.layout.centerwork.horizontal,
   -- lain.layout.termfair,
   -- lain.layout.termfair.center,
}
awful.util.taglist_buttons = awful.util.table.join(
   awful.button({ }, 1, function(t) t:view_only() end),
   awful.button({ modkey }, 1, function(t)
         if client.focus then
            client.focus:move_to_tag(t)
         end
   end),
   awful.button({ }, 3, awful.tag.viewtoggle),
   awful.button({ modkey }, 3, function(t)
         if client.focus then
            client.focus:toggle_tag(t)
         end
   end),
   awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
   awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)
awful.util.tasklist_buttons = awful.util.table.join(
   awful.button({ }, 1, function (c)
         if c == client.focus then
            c.minimized = true
         else
            -- Without this, the following
            -- :isvisible() makes no sense
            c.minimized = false
            if not c:isvisible() and c.first_tag then
               c.first_tag:view_only()
            end
            -- This will also un-minimize
            -- the client, if needed
            client.focus = c
            c:raise()
         end
   end),
   awful.button({ }, 3, function()
         local instance = nil

         return function ()
            if instance and instance.wibox.visible then
               instance:hide()
               instance = nil
            else
               instance = awful.menu.clients({ theme = { width = 250 } })
            end
         end
   end),
   awful.button({ }, 4, function ()
         awful.client.focus.byidx(1)
   end),
   awful.button({ }, 5, function ()
         awful.client.focus.byidx(-1)
end))

lain.layout.termfair.nmaster           = 3
lain.layout.termfair.ncol              = 1
lain.layout.termfair.center.nmaster    = 3
lain.layout.termfair.center.ncol       = 1
lain.layout.cascade.tile.offset_x      = 2
lain.layout.cascade.tile.offset_y      = 32
lain.layout.cascade.tile.extra_padding = 5
lain.layout.cascade.tile.nmaster       = 5
lain.layout.cascade.tile.ncol          = 2

local theme_path = string.format("%s/.config/awesome/themes/%s/theme.lua", os.getenv("HOME"), chosen_theme)
beautiful.init(theme_path)
-- }}}

-- {{{ Menu
local myawesomemenu = {
   { "hotkeys", function() return false, hotkeys_popup.show_help end },
   { "manual", terminal .. " -e 'man awesome'" },
   { "edit config", string.format("%s %s", gui_editor, awesome.conffile) },
   { "restart", awesome.restart },
   { "suspend", function ()
        awful.spawn("loginctl suspend")
   end},
   { "quit", function() awesome.quit() end }
}
awful.util.mymainmenu = freedesktop.menu.build({
      icon_size = beautiful.menu_height or 16,
      before = {
         { "Awesome", myawesomemenu, beautiful.awesome_icon },
         -- other triads can be put here
      },
      after = {
         { "Open terminal", terminal },
         -- other triads can be put here
      }
})
--menubar.utils.terminal = terminal -- Set the Menubar terminal for applications that require it
-- }}}

-- {{{ Screen
-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", function(s)
                         -- Wallpaper
                         if beautiful.wallpaper then
                            local wallpaper = beautiful.wallpaper
                            -- If wallpaper is a function, call it with the screen
                            if type(wallpaper) == "function" then
                               wallpaper = wallpaper(s)
                            end
                            gears.wallpaper.maximized(wallpaper, s, true)
                         end
end)
-- Create a wibox for each screen and add it
awful.screen.connect_for_each_screen(function(s) beautiful.at_screen_connect(s) end)
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
                awful.button({ }, 3, function () awful.util.mymainmenu:toggle() end),
                awful.button({ }, 4, awful.tag.viewnext),
                awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
   -- Take a screenshot
   -- https://github.com/copycat-killer/dots/blob/master/bin/screenshot
   awful.key({ }, "Print", function() awful.util.spawn("xfce4-screenshooter") end),
   awful.key({"Ctrl", altkey}, "Print", function() awful.util.spawn("xfce4-screenshooter -f") end),
   awful.key({"Ctrl"}, "Print", function() awful.util.spawn("xfce4-screenshooter -r") end),
   awful.key({"Ctrl", "Shift", altkey}, "Print", function() awful.util.spawn("xfce4-screenshooter -f -c") end),
   awful.key({"Ctrl", "Shift"}, "Print", function() awful.util.spawn("xfce4-screenshooter -r -c") end),
   awful.key({altkey}, "Print", function() awful.util.spawn("xfce4-screenshooter -w") end),
   awful.key({altkey, "Shift"}, "Print", function() awful.util.spawn("xfce4-screenshooter -w -c") end),

   -- Applications
   -- d... dired?
   awful.key({modkey}, "d", function() awful.util.spawn("thunar") end,
      {description = "open file manager", group = "applications"}),

   awful.key({ }, "XF86WebCam",
      function() awful.util.spawn("cheese") end,
      {description = "open webcam", group = "applications"}),

   awful.key({modkey}, "t",
      function() awful.util.spawn("telegram-desktop") end,
      {description = "open telegram", group = "applications"}),

   -- Open the rc config on the default gui_editor
   awful.key({modkey, "Control"}, "e",
      function () awful.util.spawn(string.format("%s %s", gui_editor, awesome.conffile)) end,
      {description = "open file config of awesome", group = "applications"}),



   -- User programs
   awful.key({ modkey }, "q", function () awful.spawn(browser) end,
      { description = "open browser", group = "applications"}),
   awful.key({ modkey }, "e", function () awful.spawn(gui_editor) end,
      { description = "open editor", group = "applications"}),

   -- Menubar
   awful.key({ modkey }, "p",
      function() awful.spawn("rofi -show drun -async-pre-load 100") end,
      {description = "show the menubar", group = "launcher"}),
   -- Rofi client
   awful.key({ modkey, "Ctrl" }, "p",
      function() awful.spawn("rofi -show combi -async-pre-load 100") end,
      {description = "show the window menu", group = "client"}),
   -- Hotkeys
   awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
      {description="show help", group="awesome"}),
   -- Tag browsing
   awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
      {description = "view previous", group = "tag"}),
   awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
      {description = "view next", group = "tag"}),
   awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
      {description = "go back", group = "tag"}),

   -- Non-empty tag browsing
   awful.key({ modkey, altkey }, "Left", function () lain.util.tag_view_nonempty(-1) end,
      {description = "view  previous nonempty", group = "tag"}),
   awful.key({ modkey, altkey }, "Right", function () lain.util.tag_view_nonempty(1) end,
      {description = "view  previous nonempty", group = "tag"}),

   -- Default client focus
   awful.key({ modkey, altkey,           }, "n",
      function ()
         awful.client.focus.byidx( 1)
      end,
      {description = "focus next by index", group = "client"}
   ),
   awful.key({ modkey, altkey,           }, "p",
      function ()
         awful.client.focus.byidx(-1)
      end,
      {description = "focus previous by index", group = "client"}
   ),

   -- By direction client focus
   awful.key({ modkey }, "j",
      function()
         awful.client.focus.bydirection("down")
         if client.focus then client.focus:raise() end
   end),
   awful.key({ modkey }, "k",
      function()
         awful.client.focus.bydirection("up")
         if client.focus then client.focus:raise() end
   end),
   awful.key({ modkey }, "h",
      function()
         awful.client.focus.bydirection("left")
         if client.focus then client.focus:raise() end
   end),
   awful.key({ modkey }, "l",
      function()
         awful.client.focus.bydirection("right")
         if client.focus then client.focus:raise() end
   end),
   awful.key({ modkey,           }, "w", function () awful.util.mymainmenu:show() end,
      {description = "show main menu", group = "awesome"}),

   -- Layout manipulation
   awful.key({ modkey, altkey  }, "f", function () awful.client.swap.byidx(  1)    end,
      {description = "swap with next client by index", group = "client"}),
   awful.key({ modkey, altkey   }, "b", function () awful.client.swap.byidx( -1)    end,
      {description = "swap with previous client by index", group = "client"}),
   awful.key({ modkey, "Control" }, "l", function () awful.screen.focus_relative( 1) end,
      {description = "focus the next screen", group = "screen"}),
   awful.key({ modkey, "Control" }, "o", function () awful.screen.focus_relative( 1) end,
      {description = "focus the next screen", group = "screen"}),
   awful.key({ modkey, "Control" }, "h", function () awful.screen.focus_relative(-1) end,
      {description = "focus the previous screen", group = "screen"}),
   awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
      {description = "jump to urgent client", group = "client"}),
   -- Cycle Window
   -- modkey+Tab: cycle through all clients.
   awful.key({ modkey }, "Tab", function(c)
         cyclefocus.cycle({modifier="Super_L"})
   end),
   -- modkey+Shift+Tab: backwards
   awful.key({ modkey, "Shift" }, "Tab", function(c)
         cyclefocus.cycle({modifier="Super_L"})
   end),
   -- altkey+Tab: cycle through all clients.
   awful.key({ altkey }, "Tab", function(c)
         cyclefocus.cycle({modifier="Alt_L"})
   end),
   -- altkey+Shift+Tab: backwards
   awful.key({ altkey, "Shift" }, "Tab", function(c)
         cyclefocus.cycle({modifier="Alt_L"})
   end),
   -- Show/Hide Wibox
   awful.key({ modkey }, "b", function ()
         for s in screen do
            s.mywibox.visible = not s.mywibox.visible
            if s.mybottomwibox then
               s.mybottomwibox.visible = not s.mybottomwibox.visible
            end
         end
   end),

   -- On the fly useless gaps change
   awful.key({ altkey, "Control" }, "=", function () lain.util.useless_gaps_resize(1) end),
   awful.key({ altkey, "Control" }, "-", function () lain.util.useless_gaps_resize(-1) end),

   -- Dynamic tagging
   awful.key({ modkey, "Shift" }, "n", function () lain.util.add_tag() end),
   awful.key({ modkey, "Shift" }, "r", function () lain.util.rename_tag() end),
   awful.key({ modkey, "Shift" }, "Left", function () lain.util.move_tag(-1) end),  -- move to previous tag
   awful.key({ modkey, "Shift" }, "Right", function () lain.util.move_tag(1) end),  -- move to next tag
   awful.key({ modkey, "Shift" }, "d", function () lain.util.delete_tag() end),

   -- Standard program
   awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
      {description = "open a terminal", group = "launcher"}),
      awful.key({ modkey,   "Ctrl"  }, "Return", function () awful.spawn(alt_terminal) end,
      {description = "open a alternative terminal", group = "launcher"}),
   awful.key({ modkey, "Control" }, "r", awesome.restart,
      {description = "reload awesome", group = "awesome"}),
   awful.key({ modkey, "Shift"   }, "q", awesome.quit,
      {description = "quit awesome", group = "awesome"}),

   awful.key({ altkey, "Shift"   }, "l",     function () awful.tag.incmwfact( 0.05)          end,
      {description = "increase master width factor", group = "layout"}),
   awful.key({ altkey, "Shift"   }, "h",     function () awful.tag.incmwfact(-0.05)          end,
      {description = "decrease master width factor", group = "layout"}),
   awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
      {description = "increase the number of master clients", group = "layout"}),
   awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
      {description = "decrease the number of master clients", group = "layout"}),
   awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
      {description = "increase the number of columns", group = "layout"}),
   awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
      {description = "decrease the number of columns", group = "layout"}),
   awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
      {description = "select next", group = "layout"}),
   awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
      {description = "select previous", group = "layout"}),

   awful.key({ modkey, "Control" }, "n",
      function ()
         local c = awful.client.restore()
         -- Focus restored client
         if c then
            client.focus = c
            c:raise()
         end
      end,
      {description = "restore minimized", group = "client"}),

   -- Dropdown application

   awful.key({ modkey, }, "z", function ()
         awful.screen.focused().quake:toggle() end),
   awful.key({ }, "XF86AudioMute", function ()
         awful.spawn.with_shell("pactl set-sink-mute 1 toggle || pactl set-sink-mute 0 toggle") end),
   awful.key({ }, "XF86AudioMicMute", function ()
         awful.spawn.with_shell("pactl set-source-mute 1 toggle") end),
   awful.key({ }, "XF86Launch1", function ()
         awful.spawn.with_shell("dm-tool lock") end),
   -- Widgets popups
   -- awful.key({ altkey, }, "c", function () lain.widget.calendar.show(7) end),
   awful.key({ altkey, }, "h", function () if beautiful.fs then beautiful.fs.show(7) end end),
      -- awful.key({ altkey, }, "w", function () if beautiful.weather then beautiful.weather.show(7) end end),

      -- Brightess
      awful.key({ }, "XF86MonBrightnessUp", function () awful.util.spawn("xbacklight -inc 10") end),
      awful.key({ }, "XF86MonBrightnessDown", function () awful.util.spawn("xbacklight -dec 10") end),

      -- ALSA volume control (disabled because I use emacs and this key bindings is shitty too)
      -- awful.key({ altkey }, "Up",
      --     function ()
      --         os.execute(string.format("amixer -q set %s 1%%+", beautiful.volume.channel))
      --         beautiful.volume.update()
      --     end),
      -- awful.key({ altkey }, "Down",
      --     function ()
      --         os.execute(string.format("amixer -q set %s 1%%-", beautiful.volume.channel))
      --         beautiful.volume.update()
      --     end),
      -- awful.key({ altkey }, "m",
      --     function ()
      --         os.execute(string.format("amixer -q set %s toggle", beautiful.volume.togglechannel or beautiful.volume.channel))
      --         beautiful.volume.update()
      --     end),
      -- awful.key({ altkey, "Control" }, "m",
      --     function ()
      --         os.execute(string.format("amixer -q set %s 100%%", beautiful.volume.channel))
      --         beautiful.volume.update()
      --     end),

      -- awful.key({ modkey, altkey, "Control" }, "0",
      --    function ()
      --       os.execute(string.format("amixer -q set %s 0%%", beautiful.volume.channel))
      --       beautiful.volume.update()
      -- end),

      -- -- MPD control
      -- awful.key({ altkey, "Control" }, "Up",
      --    function ()
      --       awful.spawn.with_shell("mpc toggle")
      --       beautiful.mpd.update()
      -- end),
      -- awful.key({ altkey, "Control" }, "Down",
      --    function ()
      --       awful.spawn.with_shell("mpc stop")
      --       beautiful.mpd.update()
      -- end),
      -- awful.key({ altkey, "Control" }, "Left",
      --    function ()
      --       awful.spawn.with_shell("mpc prev")
      --       beautiful.mpd.update()
      -- end),
      -- awful.key({ altkey, "Control" }, "Right",
      --    function ()
      --       awful.spawn.with_shell("mpc next")
      --       beautiful.mpd.update()
      -- end),
      -- awful.key({ altkey }, "0",
      --    function ()
      --       local common = { text = "MPD widget ", position = "top_middle", timeout = 2 }
      --       if beautiful.mpd.timer.started then
      --          beautiful.mpd.timer:stop()
      --          common.text = common.text .. lain.util.markup.bold("OFF")
      --       else
      --          beautiful.mpd.timer:start()
      --          common.text = common.text .. lain.util.markup.bold("ON")
      --       end
      --       naughty.notify(common)
      -- end),

      -- Rhythmbox
      awful.key({modkey},  "XF86AudioPlay",
         function ()
            awful.spawn("rhythmbox")
         end,
         {description = "open media player", group = "applications"}
      ),

       -- Rhythmbox
      awful.key({"Ctrl"},  "XF86AudioPlay",
         function ()
            awful.spawn("rhythmbox")
         end,
         {description = "open media player", group = "applications"}
      ),

      -- Geary
      awful.key({modkey}, "a",
         function ()
            awful.spawn("geary")
         end,
         {description = "open email client", group = "applications"}
      ),

      -- Copy primary to clipboard (terminals to gtk)
      awful.key({ modkey }, "c", function () awful.spawn("xclip | xclip -i -b") end),
      -- Copy clipboard to primary (gtk to terminals)
      awful.key({ modkey }, "v", function () awful.spawn("xclip -b | xclip") end),



      -- dmenu (DON ACTIVE THIS CODE: HE IS EVIL)
      -- awful.key({ modkey }, "x", function ()
      --     awful.spawn(string.format("dmenu_run -i -fn 'Monospace' -nb '%s' -nf '%s' -sb '%s' -sf '%s'",
      --     beautiful.bg_normal, beautiful.fg_normal, beautiful.bg_focus, beautiful.fg_focus))
      -- end)
      --

      -- Prompt
      awful.key({ modkey }, "r", function () awful.spawn("rofi -show run") end,
         {description = "run prompt", group = "launcher"}),

      awful.key({ modkey }, "x",
         function ()
            awful.prompt.run {
               prompt       = "Run Lua code: ",
               textbox      = awful.screen.focused().mypromptbox.widget,
               exe_callback = awful.util.eval,
               history_path = awful.util.get_cache_dir() .. "/history_eval"
            }
         end,
         {description = "lua execute prompt", group = "awesome"}),

      awful.key({ modkey, "Ctrl" }, "x",
         function () awful.spawn("xkill") end,
         {description = "lua execute prompt", group = "awesome"})


)

clientkeys = awful.util.table.join(
   awful.key({ altkey, "Shift"   }, "m",      lain.util.magnify_client                         ),
   awful.key({ modkey,           }, "f",
      function (c)
         c.fullscreen = not c.fullscreen
         c:raise()
      end,
      {description = "toggle fullscreen", group = "client"}),

   awful.key({ altkey,           }, "F11",
      function (c)
         c.fullscreen = not c.fullscreen
         c:raise()
      end,
      {description = "toggle fullscreen", group = "client"}),

   awful.key({ modkey, "Ctrl" }, "c", awful.placement.centered,
      {description = "align to center", group = "client"}
   ),
   awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
      {description = "close", group = "client"}),
   awful.key({ altkey}, "F4",      function (c) c:kill()                         end,
      {description = "close", group = "client"}),
   awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
      {description = "toggle floating", group = "client"}),
   awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
      {description = "move to master", group = "client"}),
   awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
      {description = "move to screen", group = "client"}),
   awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
      {description = "toggle keep on top", group = "client"}),
   awful.key({ modkey,           }, "n",
      function (c)
         -- The client currently has the input focus, so it cannot be
         -- minimized, since minimized clients can't have the focus.
         c.minimized = true
      end ,
      {description = "minimize", group = "client"}),
   awful.key({ modkey,           }, "m",
      function (c)
         c.maximized = not c.maximized
         c:raise()
      end ,
      {description = "maximize", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
   globalkeys = awful.util.table.join(globalkeys,
                                      -- View tag only.
                                      awful.key({ modkey }, "#" .. i + 9,
                                         function ()
                                            local screen = awful.screen.focused()
                                            local tag = screen.tags[i]
                                            if tag then
                                               tag:view_only()
                                            end
                                         end,
                                         {description = "view tag #"..i, group = "tag"}),
                                      -- Toggle tag display.
                                      awful.key({ modkey, "Control" }, "#" .. i + 9,
                                         function ()
                                            local screen = awful.screen.focused()
                                            local tag = screen.tags[i]
                                            if tag then
                                               awful.tag.viewtoggle(tag)
                                            end
                                         end,
                                         {description = "toggle tag #" .. i, group = "tag"}),
                                      -- Move client to tag.
                                      awful.key({ modkey, "Shift" }, "#" .. i + 9,
                                         function ()
                                            if client.focus then
                                               local tag = client.focus.screen.tags[i]
                                               if tag then
                                                  client.focus:move_to_tag(tag)
                                               end
                                            end
                                         end,
                                         {description = "move focused client to tag #"..i, group = "tag"}),
                                      -- Toggle tag on focused client.
                                      awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                                         function ()
                                            if client.focus then
                                               local tag = client.focus.screen.tags[i]
                                               if tag then
                                                  client.focus:toggle_tag(tag)
                                               end
                                            end
                                         end,
                                         {description = "toggle focused client on tag #" .. i, group = "tag"})
   )
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen + awful.placement.centered,
                     size_hints_honor = false
     }
    },

    -- Titlebars
    { rule_any = { type = { "dialog", "normal" } },
      properties = { titlebars_enabled = false } },

    -- Set Firefox to always map on the first tag on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = awful.util.tagnames[1] } },

    { rule = { class = "Gimp", role = "gimp-image-window" },
          properties = { maximized = true } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup and
      not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
         awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- Custom
    if beautiful.titlebar_fun then
        beautiful.titlebar_fun(c)
        return
    end

    -- Default
    -- buttons for the titlebar
    local buttons = awful.util.table.join(
        awful.button({ }, 1, function()
            client.focus = c
            c:raise()
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            client.focus = c
            c:raise()
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c, {size = 16}) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
        and awful.client.focus.filter(c) then
        client.focus = c
    end
end)

-- No border for maximized clients
client.connect_signal("focus",
    function(c)
        if c.maximized then -- no borders if only 1 client visible
            c.border_width = 0
        elseif #awful.screen.focused().clients > 1 then
            c.border_width = beautiful.border_width
            c.border_color = beautiful.border_focus
        end
    end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

-- make rofi possible to access minimized clients
client.connect_signal("request::activate",
                      function(c, context, hints)
                         if c.minimized then
                            c.minimized = false
                         end
                         awful.ewmh.activate(c, context, hints)
                      end)
