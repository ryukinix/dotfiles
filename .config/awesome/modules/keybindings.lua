-- modules
require("modules.config")
local awesome, client = awesome, client
local cyclefocus    = require("cyclefocus")
local hotkeys_popup = require("awful.hotkeys_popup").widget
local spotify_shell = require("awesome-wm-widgets.spotify-shell.spotify-shell")
local lain          = require("lain")
local awful         = require("awful")
local beautiful     = require("beautiful")


-- {{{ Mouse bindings
root.buttons(
   awful.util.table.join(
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
      function() awful.spawn("env XDG_CURRENT_DESKTOP=XFCE rofi -show combi -async-pre-load 100") end,
      {description = "show the window menu", group = "client"}),
   awful.key({ modkey, "Shift" }, "p",
      function() awful.spawn("rofi -show window -async-pre-load 100") end,
      {description = "show open windows", group = "client"}),
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
      {description = "view previous nonempty", group = "tag"}),
   awful.key({ modkey, altkey }, "Right", function () lain.util.tag_view_nonempty(1) end,
      {description = "view previous nonempty", group = "tag"}),

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
         awful.spawn.with_shell(locker) end),
   awful.key({modkey}, "l", function ()
         awful.spawn.with_shell(locker) end),


   -- multimedia keyboard shortcuts
   awful.key({ modkey,   "Control"  }, "s", function () spotify_shell.launch() end, {description = "spotify shell", group = "music"}),
   awful.key({ }, "XF86AudioPlay", function () awful.util.spawn("dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause", false) end),
   awful.key({ }, "XF86AudioNext", function () awful.util.spawn("dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next", false) end),
   awful.key({ }, "XF86AudioPrev", function () awful.util.spawn("dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous", false) end),
   awful.key({ }, "XF86AudioStop", function () awful.util.spawn("dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Stop", false) end),

   -- Widgets popups
   -- awful.key({ altkey, }, "c", function () lain.widget.calendar.show(7) end),
   awful.key({ altkey, }, "h", function () if beautiful.fs then beautiful.fs.show(7) end end),
      -- awful.key({ altkey, }, "w", function () if beautiful.weather then beautiful.weather.show(7) end end),

      -- Brightess
   awful.key({ }, "XF86MonBrightnessUp", function () awful.util.spawn("xbacklight -inc 10") end),
   awful.key({ }, "XF86MonBrightnessDown", function () awful.util.spawn("xbacklight -dec 10") end),
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
   globalkeys = awful.util.table.join(
      globalkeys,
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
