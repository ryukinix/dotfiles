local wibox = require("wibox")
local awful = require("awful")
local naughty = require("naughty")
local watch = require("awful.widget.watch")

local connect_command = "openvpn3 session-start --config /etc/openvpn3/neoway.ovpn;"
local disconnect_command = "openvpn3 sessions-list | grep Path | cut -d : -f 2 | tr -d ' ' | xargs -P 5 -I@ openvpn3 session-manage --session-path @ --disconnect;"


vpn_widget = wibox.widget.textbox()
vpn_widget.vpn_on = false
vpn_widget:connect_signal(
   "button::press",
   function(_, _, _, button)
      if button == 1 then
         local cmd = "notify-send VPN connecting;" .. connect_command .. "notify-send VPN connected"
         if vpn_widget.vpn_on ==  true then
            cmd = "notify-send VPN disconnecting;" .. disconnect_command .. "notify-send VPN disconnected"
         end
         awful.spawn.with_shell(cmd)
      end
   end
)

function update_vpn_status(widget, stdout, stderr, exitreason, exitcode)
   if(stdout == '' or stdout==nil or stdout=='Device "tun0" does not exist.') then
      widget.vpn_on = false
      widget.text= "🖧🛡"
   else
      widget.vpn_on = true
      widget.text= "🖧⛨"
   end
end

watch(
   "ip addr show tun0", 2,
   update_vpn_status,
   vpn_widget
)

return vpn_widget
