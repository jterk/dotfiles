local grid = require "hs.grid"
local hotkey = require "hs.hotkey"

grid.MARGINX = 0
grid.MARGINY = 0
grid.GRIDWIDTH = 2
grid.GRIDHEIGHT = 2

-- a helper function that returns another function that resizes the current window
-- to a certain grid size.
local gridset = function(x, y, w, h)
    return function()
        cur_window = hs.window.focusedWindow()
        grid.set(
            cur_window,
            {x=x, y=y, w=w, h=h},
            cur_window:screen()
        )
    end
end

local mash = {"ctrl", "shift"}
hotkey.bind(mash, 'n', grid.pushWindowNextScreen)
hotkey.bind(mash, 's', grid.maximizeWindow) -- full screen
hotkey.bind(mash, 'a', gridset(0, 0, 1, 2)) -- left half
hotkey.bind(mash, 'd', gridset(1, 0, 1, 2)) -- right half
hotkey.bind(mash, 'w', gridset(0, 0, 2, 1)) -- top half
hotkey.bind(mash, 'x', gridset(0, 1, 2, 1)) -- bottom half