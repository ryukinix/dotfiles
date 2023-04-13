--[[

   Multicolor Awesome WM theme 2.0
   github.com/copycat-killer

--]]


local gears = require("gears")
local lain  = require("lain")
local awful = require("awful")
local wibox = require("wibox")
local watch = require("awful.widget.watch")
local net_widgets = require("net_widgets")
local logout_menu_widget = require("awesome-wm-widgets.logout-menu-widget.logout-menu")
local calendar_widget = require("awesome-wm-widgets.calendar-widget.calendar")
local fs_widget = require("awesome-wm-widgets.fs-widget.fs-widget")
-- you need: luarocks install lua-json
local jira_widget = require("awesome-wm-widgets.jira-widget.jira")
local vpn = require("modules.vpn")
local spotify_widget = require("awesome-wm-widgets.spotify-widget.spotify")
local net_speed_widget = require("awesome-wm-widgets.net-speed-widget.net-speed")
local cpu_widget = require("awesome-wm-widgets.cpu-widget.cpu-widget")
local gitlab_widget = require("awesome-wm-widgets.gitlab-widget.gitlab")

local os    = { getenv = os.getenv, setlocale = os.setlocale }

local theme                                     = {}
theme.lain_icons                                = os.getenv("HOME") .. "/.config/awesome/lain/icons/layout/zenburn/"
theme.layout_termfair                           = theme.lain_icons .. "termfair.png"
theme.layout_centerfair                         = theme.lain_icons .. "centerfair.png"  -- termfair.center
theme.layout_cascade                            = theme.lain_icons .. "cascade.png"
theme.layout_cascadetile                        = theme.lain_icons .. "cascadetile.png" -- cascade.tile
theme.layout_centerwork                         = theme.lain_icons .. "centerwork.png"
theme.layout_centerworkh                        = theme.lain_icons .. "centerworkh.png" -- centerwork.horizontal
theme.confdir                                   = os.getenv("HOME") .. "/.config/awesome/themes/multicolor"
theme.wallpaper                                 = theme.confdir .. "/wall.jpg"
theme.font                                      = "Terminus (TTF) 12"
theme.menu_bg_normal                            = "#000000"
theme.menu_bg_focus                             = "#000000"
theme.bg_normal                                 = "#000000"
theme.bg_focus                                  = "#000000"
theme.bg_urgent                                 = "#000000"
theme.fg_normal                                 = "#aaaaaa"
theme.fg_focus                                  = "#3ca4d8" -- "#ff8c00"
theme.fg_urgent                                 = "#af1d18"
theme.fg_minimize                               = "#ffffff"
theme.border_width                              = 1
theme.border_normal                             = "#1c2022"
theme.border_focus                              = "#606060"
theme.border_marked                             = "#3ca4d8"
theme.menu_border_width                         = 0
theme.menu_width                                = 130
theme.menu_submenu_icon                         = theme.confdir .. "/icons/submenu.png"
theme.menu_fg_normal                            = "#aaaaaa"
theme.menu_fg_focus                             = "#3ca4d8" -- "#ff8c00"
theme.menu_bg_normal                            = "#050505dd"
theme.menu_bg_focus                             = "#050505dd"
theme.widget_temp                               = theme.confdir .. "/icons/temp.png"
theme.widget_uptime                             = theme.confdir .. "/icons/ac.png"
theme.widget_cpu                                = theme.confdir .. "/icons/cpu.png"
theme.widget_weather                            = theme.confdir .. "/icons/dish.png"
theme.widget_fs                                 = theme.confdir .. "/icons/fs.png"
theme.widget_mem                                = theme.confdir .. "/icons/mem.png"
theme.widget_fs                                 = theme.confdir .. "/icons/fs.png"
theme.widget_note                               = theme.confdir .. "/icons/note.png"
theme.widget_note_on                            = theme.confdir .. "/icons/note_on.png"
theme.widget_netdown                            = theme.confdir .. "/icons/net_down.png"
theme.widget_netup                              = theme.confdir .. "/icons/net_up.png"
theme.widget_mail                               = theme.confdir .. "/icons/mail.png"
theme.widget_batt                               = theme.confdir .. "/icons/bat.png"
theme.widget_clock                              = theme.confdir .. "/icons/clock.png"
theme.widget_vol                                = theme.confdir .. "/icons/spkr.png"
theme.taglist_squares_sel                       = theme.confdir .. "/icons/square_a.png"
theme.taglist_squares_unsel                     = theme.confdir .. "/icons/square_b.png"
theme.tasklist_plain_task_name                  = true
theme.tasklist_disable_icon                     = true
theme.useless_gap                               = 5
theme.layout_tile                               = theme.confdir .. "/icons/tile.png"
theme.layout_tilegaps                           = theme.confdir .. "/icons/tilegaps.png"
theme.layout_tileleft                           = theme.confdir .. "/icons/tileleft.png"
theme.layout_tilebottom                         = theme.confdir .. "/icons/tilebottom.png"
theme.layout_tiletop                            = theme.confdir .. "/icons/tiletop.png"
theme.layout_fairv                              = theme.confdir .. "/icons/fairv.png"
theme.layout_fairh                              = theme.confdir .. "/icons/fairh.png"
theme.layout_spiral                             = theme.confdir .. "/icons/spiral.png"
theme.layout_dwindle                            = theme.confdir .. "/icons/dwindle.png"
theme.layout_max                                = theme.confdir .. "/icons/max.png"
theme.layout_fullscreen                         = theme.confdir .. "/icons/fullscreen.png"
theme.layout_magnifier                          = theme.confdir .. "/icons/magnifier.png"
theme.layout_floating                           = theme.confdir .. "/icons/floating.png"
theme.titlebar_close_button_normal              = theme.confdir .. "/icons/titlebar/close_normal.png"
theme.titlebar_close_button_focus               = theme.confdir .. "/icons/titlebar/close_focus.png"
theme.titlebar_minimize_button_normal           = theme.confdir .. "/icons/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus            = theme.confdir .. "/icons/titlebar/minimize_focus.png"
theme.titlebar_ontop_button_normal_inactive     = theme.confdir .. "/icons/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive      = theme.confdir .. "/icons/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active       = theme.confdir .. "/icons/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active        = theme.confdir .. "/icons/titlebar/ontop_focus_active.png"
theme.titlebar_sticky_button_normal_inactive    = theme.confdir .. "/icons/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive     = theme.confdir .. "/icons/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active      = theme.confdir .. "/icons/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active       = theme.confdir .. "/icons/titlebar/sticky_focus_active.png"
theme.titlebar_floating_button_normal_inactive  = theme.confdir .. "/icons/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive   = theme.confdir .. "/icons/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active    = theme.confdir .. "/icons/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active     = theme.confdir .. "/icons/titlebar/floating_focus_active.png"
theme.titlebar_maximized_button_normal_inactive = theme.confdir .. "/icons/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = theme.confdir .. "/icons/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active   = theme.confdir .. "/icons/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active    = theme.confdir .. "/icons/titlebar/maximized_focus_active.png"

local markup = lain.util.markup

function get_file_contents(fname)
   local fname = string.gsub(fname, "~", os.getenv("HOME"))
   local f = assert(io.open(fname, "r"))
   local content = f:read("*all")
   f:close()
   return content:match("[^\n]*")
end

function get_hostname()
   return get_file_contents("/etc/hostname")
end

local hostname = get_hostname()

-- Textclock
os.setlocale(os.getenv("LANG")) -- to localize the clock
local clockicon = wibox.widget.imagebox(theme.widget_clock)
local mytextclock = wibox.widget.textclock(
   markup("#7788af", "%d/%m/%Y ") .. markup("#535f7a", ">") .. markup("#3ca4d8", " %H:%M ")
)
mytextclock.font = theme.font

-- Calendar
-- default
local cw = calendar_widget()
-- or customized
local cw = calendar_widget({
      theme = 'dark',
      placement = 'top_right',
      start_sunday = true,
      radius = 8,
      -- with customized next/previous (see table above)
      previous_month_button = 1,
      next_month_button = 3,
})
mytextclock:connect_signal(
   "button::press",
   function(_, _, _, button)
      if button == 1 then cw.toggle() end
   end
)


theme.cal = cw


-- CPU
local cpuicon = wibox.widget.imagebox(theme.widget_cpu)
local cpu = lain.widget.cpu({
      settings = function()
         widget:set_markup(markup.fontfg(theme.font, "#e33a6e", string.format("%02d%% ", cpu_now.usage)))
      end
})

-- Coretemp
local tempicon = wibox.widget.imagebox(theme.widget_temp)
local temp = lain.widget.temp({
      settings = function()
         if coretemp_now ~= "N/A" then
            widget:set_markup(markup.fontfg(theme.font, "#f1af5f", coretemp_now .. "°C "))
         end
      end
})

-- Battery
local baticon = wibox.widget.imagebox(theme.widget_batt)
local bat = lain.widget.bat({
      settings = function()
         local perc = bat_now.perc ~= "N/A" and bat_now.perc .. "%" or bat_now.perc

         if bat_now.status == "Charging" then
            perc = "+" .. perc
         end

         widget:set_markup(markup.fontfg(theme.font, theme.fg_normal, perc .. " "))
      end
})


local interface_name = "wlan0"
if hostname == "PC-002653" then
   interface_name = "wlp5s0"
end

local net_indicator = net_widgets.wireless({
      interface=interface_name,
      font=theme.font
})

-- vpn
vpn.font = theme.font

-- Memory RAM
local memicon = wibox.widget.imagebox(theme.widget_mem)
local memory = lain.widget.mem({
      settings = function()

         local m = mem_now.used .. "M "
         local k = tonumber(mem_now.used)
         if k < 1024 then
            m = string.format("%04.fMB ", k)
         else
            m = string.format("%04.1fGB ", k/1024)
         end

         widget:set_markup(markup.fontfg(theme.font, "#e0da37", m))
      end
})

local jira = jira_widget({
      host = 'https://neoway.atlassian.net',
      query = 'jql=assignee=currentuser()+AND+statusCategory!=done'}
)
local gitlab = gitlab_widget{
   host = "https://gitlab.neoway.com.br",
   access_token = get_file_contents("~/.gitlab.neoway.token")
}
gitlab:set_visible(false)

vpn:connect_signal(
   "button::press",
   function(_, _, _, button)
      if button == 1 then
         if vpn.vpn_on == false then
            gitlab:set_visible(true)
         else
            gitlab:set_visible(false)
         end
      end
   end
)

function theme.at_screen_connect(s)
   -- Quake application
   s.quake = lain.util.quake({ app = awful.util.terminal })
   -- If wallpaper is a function, call it with the screen
   local wallpaper = theme.wallpaper
   if type(wallpaper) == "function" then
      wallpaper = wallpaper(s)
   end
   gears.wallpaper.maximized(wallpaper, s, true)

   -- Tags
   awful.tag(awful.util.tagnames, s, awful.layout.layouts)

   -- Create a promptbox for each screen
   s.mypromptbox = awful.widget.prompt()
   -- Create an imagebox widget which will contains an icon indicating which layout we're using.
   -- We need one layoutbox per screen.
   s.mylayoutbox = awful.widget.layoutbox(s)
   s.mylayoutbox:buttons(awful.util.table.join(
                            awful.button({ }, 1, function () awful.layout.inc( 1) end),
                            awful.button({ }, 3, function () awful.layout.inc(-1) end),
                            awful.button({ }, 4, function () awful.layout.inc( 1) end),
                            awful.button({ }, 5, function () awful.layout.inc(-1) end)))
   -- Create a taglist widget
   s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, awful.util.taglist_buttons)

   -- Create a tasklist widget
   s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, awful.util.tasklist_buttons)

   -- Create the wibox
   s.mywibox = awful.wibar({ position = "top", screen = s, height = 25, bg = theme.bg_normal, fg = theme.fg_normal })

   s.rofibutton = awful.widget.launcher({command = "rofi -show drun",
                                         image = theme.menu_submenu_icon })

   -- Add widgets to the wibox
   s.mywibox:setup {
      layout = wibox.layout.align.horizontal,
      expand = "none",
      { -- Left widgets
         layout = wibox.layout.fixed.horizontal,
         expand = "left",
         s.rofibutton,
         --s.mylayoutbox,
         s.mytaglist,
         s.mypromptbox,
         net_speed_widget(),
         cpu_widget(),
         tempicon,
         temp.widget,
         baticon,
         bat.widget,
         fs_widget({ mounts = { '/', }, popup_bg = '#000000cc' }),
      },
      -- Middle widget
      spotify_widget(
         {
            font = theme.font,
            max_length = 15,
            play_icon = '/usr/share/icons/Numix-Circle/48/apps/spotify.svg',
            pause_icon = '/usr/share/icons/Numix-Light/22/status/renamed-spotify-client.svg'
         }
      ),
      {
         layout = wibox.layout.fixed.horizontal,
         expand = "right",
         wibox.widget.systray(),
         net_indicator,
         gitlab,
         jira,
         vpn,
         clockicon,
         mytextclock,
         logout_menu_widget({onlock = function() awful.spawn.with_shell("slock") end})
      }
   }

   -- Create the bottom wibox
   s.mybottomwibox = awful.wibar({ position = "bottom", screen = s,
                                   border_width = 0, height = 25,
                                   bg = theme.bg_normal, fg = theme.fg_normal })

   -- Add widgets to the bottom wibox
   s.mybottomwibox:setup {
      layout = wibox.layout.align.horizontal,
      { -- Left widgets
         layout = wibox.layout.fixed.horizontal,
      },
      s.mytasklist, -- Middle widget
      { -- Right widgets
         layout = wibox.layout.fixed.horizontal,
         s.mylayoutbox,
      },
   }
end

return theme
