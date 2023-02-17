local wibox = require("wibox")
local awful = require("awful")
local naughty = require("naughty")
local watch = require("awful.widget.watch")
local vpn_on = false

function update_vpn_status(widget, stdout, stderr, exitreason, exitcode)
   if(stdout == '' or stdout==nil or stdout=='Device "tun0" does not exist.') then
      vpn_on = false
      widget.text= "VPN[🛡]"
   else
      vpn_on = true
      widget.text= "VPN[⛨]"
   end
end

vpn_widget = wibox.widget.textbox()
vpn_widget:connect_signal(
   "button::press",
   function(_, _, _, button)
      if button == 1 then
         if vpn_on ==  false then
            awful.spawn.with_shell("notify-send VPN connecting; openvpn3 session-start --config /etc/openvpn3/neoway.ovpn; notify-send VPN connected")
         else
            awful.spawn.with_shell("openvpn3 session-manage --disconnect --config /etc/openvpn3/neoway.ovpn; notify-send VPN disconnected")
         end
      end
   end
)

watch(
   "ip addr show tun0", 2,
   update_vpn_status,
   vpn_widget
)

return vpn_widget
