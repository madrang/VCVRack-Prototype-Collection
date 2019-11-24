-- Voltage Scaler by Madrang
-- Scale input and apply offset with low/high pass filters
-- Has 2 groups of 3 inputs
-- Group 1 - In 1-3/Out 1-3 - K1 = Offset, K2 = Gain, K3 = Filter
-- Group 2 - In 4-6/Out 4-6 - K4 = Offset, K5 = Gain, K6 = Filter
-- Modes
--   Disabled = Output 0 Volts
--   Offset = Output Constant Voltage
--   Scale:DC = Output scaled input with offset and low pass filter
--   Scale:AC = Output scaled input with offset and high pass filter

-- Module Config
config.frameDivider = 1
config.bufferSize = 64

-- Constants
epsilon = 0.00001
debounceTime = 0.5
voltsMaximum = 10.0
voltsMinimum = -10.0
voltsRange = voltsMaximum - voltsMinimum

displayUpdateDelay = 0.500
displayModeDelay = 5.0

-- Vars

-- Store last read value.
-- Detect changes on input read.
knobsInputBuffer = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }

-- Debounce timer, Disable Switch for a set amount of time.
switchDisableTimer = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }

-- Display timers, Display a value after change.
knobsDisplayTimer = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }
switchDisplayTimer = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }

-- Timer to next display update
displayUpdateTimer = 0.0
-- Initial message after start
welcomeMsg = "Scaler:V0.0.1 -- Gr1:1-3 Gr2:4-6\n" .. 
	"- Gr1: K1=Offset K2=Gain K3=Hyst\n" ..
	"- Gr2: K4=Offset K5=Gain K6=Hyst"

-- Display Functions Table, Select Display mode.
-- Call the function of the last moddified input.
knobsDisplayFunctions = {}
switchesDisplayFunctions = {}

-- Input Function Table, Store param on input change.
knobsInputFunctions = {}
switchesInputFunctions = {}

rowIOModes = { 2, 2, 2, 2, 2, 2 }
rowModeMute = 0
rowModeVolt = 1
rowModeDC = 2
rowModeAC = 3
rowIOModesNamesLong = {
	[rowModeMute] = "Disabled",
	[rowModeVolt] = "Offset",
	[rowModeDC] = "Scale:DC",
	[rowModeAC] = "Scale:AC",
}
rowIOModesNames = {
	[rowModeMute] = "--",
	[rowModeVolt] = "CV",
	[rowModeDC] = "DC",
	[rowModeAC] = "AC",
}
rowIOModesNbr = table.getn(rowIOModesNames)

lightPeakGain = 2
highpassGainCurve = 12
lowpassGainCurve = 6
outputsAverage = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }
outputsLastVal = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }
inputsLastVal = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }

inputGroups = { 1, 1, 1, 2, 2, 2 }
offsetInputs = { 0.0, 0.0 }
scaleInputs = { 0.0, 0.0 }
hysteresisInputs = { 0.0, 0.0 }

phase = 0

function initDisplay()
	-- Set initial display
	display(welcomeMsg)
	-- Time to display the message.
	displayUpdateTimer = 10.0
	
	knobsDisplayFunctions = {
		[1] = displayGroup1,
		[2] = displayGroup1,
		[3] = displayGroup1,
		[4] = displayGroup2,
		[5] = displayGroup2,
		[6] = displayGroup2,
	}
	switchesDisplayFunctions = {
		[1] = displayGroup1,
		[2] = displayGroup1,
		[3] = displayGroup1,
		[4] = displayGroup2,
		[5] = displayGroup2,
		[6] = displayGroup2,
	}
end

function initInputs()
	knobsInputFunctions = {
		-- Range from Minimum voltage to Maximum Voltage
		[1] = function (x) offsetInputs[1] = x * voltsRange + voltsMinimum end,
		-- Define scale so knob at 0.5 is 1, at 0 scale is 0 and at one infinite.
		[2] = function (x) scaleInputs[1] = math.pow(x + 1.0, (1.0 / (1.0 - x)) - 0.25) - 1.0 end,
		[3] = function (x) hysteresisInputs[1] = x end,
		-- Range from Minimum voltage to Maximum Voltage
		[4] = function (x) offsetInputs[2] = x * voltsRange + voltsMinimum end,
		-- Define scale so knob at 0.5 is 1, at 0 scale is 0 and at one infinite.
		[5] = function (x) scaleInputs[2] = math.pow(x + 1.0, (1.0 / (1.0 - x)) - 0.25) - 1.0 end,
		[6] = function (x) hysteresisInputs[2] = x end,
	}
	switchesInputFunctions = {
		[1] = function () rowIOModes[1] = (rowIOModes[1] + 1) % (rowIOModesNbr + 1) end,
		[2] = function () rowIOModes[2] = (rowIOModes[2] + 1) % (rowIOModesNbr + 1) end,
		[3] = function () rowIOModes[3] = (rowIOModes[3] + 1) % (rowIOModesNbr + 1) end,
		[4] = function () rowIOModes[4] = (rowIOModes[4] + 1) % (rowIOModesNbr + 1) end,
		[5] = function () rowIOModes[5] = (rowIOModes[5] + 1) % (rowIOModesNbr + 1) end,
		[6] = function () rowIOModes[6] = (rowIOModes[6] + 1) % (rowIOModesNbr + 1) end,
	}
end

function updateDisplay(lastCallTime)
	-- Check if screen was initialised.
	if not (knobsDisplayFunctions[1]) then
		initDisplay()
		return
	end
	
	-- Update Timers
	-- Input timers
	for i=1,6 do
		if knobsDisplayTimer[i] > lastCallTime then
			-- Decrease display timer.
			knobsDisplayTimer[i] = knobsDisplayTimer[i] - lastCallTime
		elseif knobsDisplayTimer[i] > epsilon then
			-- Avoid going negative with the timer.
			knobsDisplayTimer[i] = 0.0
		end
		if switchDisplayTimer[i] > lastCallTime then
			-- Decrease display timer.
			switchDisplayTimer[i] = switchDisplayTimer[i] - lastCallTime
		elseif switchDisplayTimer[i] > epsilon then
			-- Avoid going negative with the timer.
			switchDisplayTimer[i] = 0.0
		end
	end
	-- Display timer
	if displayUpdateTimer > lastCallTime then
		-- Decrease display timer.
		displayUpdateTimer = displayUpdateTimer - lastCallTime
		-- Disable display until timer reach zero.
		return
	elseif displayUpdateTimer > epsilon then
		-- Avoid going negative with the timer.
		displayUpdateTimer = 0.0
	end
	
	-- Check inputs to find last moddified
	lastRowUpdated = 0
	rowDisplayTime = 0.0
	isKnob = false
	for i=1,6 do
		if rowDisplayTime < knobsDisplayTimer[i] then
			rowDisplayTime = knobsDisplayTimer[i]
			lastRowUpdated = i
			isKnob = true
		end
		if rowDisplayTime < switchDisplayTimer[i] then
			rowDisplayTime = switchDisplayTimer[i]
			lastRowUpdated = i
			isKnob = false
		end
	end
	
	-- Select display mode using the last moddified input.
	displayFunc = displayIdle
	if lastRowUpdated > 0 and rowDisplayTime > epsilon then
		if isKnob == true then
			displayFunc = knobsDisplayFunctions[lastRowUpdated]
		else
			displayFunc = switchesDisplayFunctions[lastRowUpdated]
		end
	end
	
	-- Update the display.
	if (displayFunc) then
		displayFunc()
		-- Set timer for next update.
		displayUpdateTimer = displayUpdateDelay
	else
		-- Unknown display mode error.
		displayModeError(lastRowUpdated, isKnob)
		-- Time to display the error.
		displayUpdateTimer = 5.0
	end
end

function displayModeError(row, isKnob)
	display("Error: Unknown display mode\n- Mode:" .. string.format("%s", row) .. "\n- IsKnob:" .. ternary(isKnob, "True", "False"))
end

function displayIdle()
	display(
		"G1: OS=" .. ternary(offsetInputs[1] >= 0, "+", "") .. string.format("%.3f", offsetInputs[1]) ..
		" GN=" .. string.format("%.3f", scaleInputs[1]) ..
		" HY=" .. string.format("%.3f", hysteresisInputs[1]) ..
		"\n" ..
		"G2: OS=" .. ternary(offsetInputs[2] >= 0, "+", "") .. string.format("%.3f", offsetInputs[2]) ..
		" GN=" .. string.format("%.3f", scaleInputs[2]) ..
		" HY=" .. string.format("%.3f", hysteresisInputs[1]) ..
		"\n" ..
		"1:" .. rowIOModesNames[rowIOModes[1]] .. " 2:" .. rowIOModesNames[rowIOModes[2]] .. " 3:" .. rowIOModesNames[rowIOModes[3]] ..
		" 4:" .. rowIOModesNames[rowIOModes[4]] .. " 5:" .. rowIOModesNames[rowIOModes[5]] .." 6:" .. rowIOModesNames[rowIOModes[6]] ..
		""
	)
end

function displayGroup1()
	display("----- Group 1 -----\n" ..
		"OS:" .. ternary(offsetInputs[1] >= 0, "+", "") .. string.format("%.3f", offsetInputs[1]) ..
		" GN:" .. string.format("%.3f", scaleInputs[1]) ..
		" HY:" .. string.format("%.3f", hysteresisInputs[1]) ..
		"\n" ..
		" 1:" .. rowIOModesNamesLong[rowIOModes[1]] ..
		" 2:" .. rowIOModesNamesLong[rowIOModes[2]] ..
		" 3:" .. rowIOModesNamesLong[rowIOModes[3]] ..
		""
	)
end

function displayGroup2()
	display("----- Group 2 -----\n" ..
		"OS:" .. ternary(offsetInputs[2] >= 0, "+", "") .. string.format("%.3f", offsetInputs[2]) ..
		" GN:" .. string.format("%.3f", scaleInputs[2]) ..
		" HY:" .. string.format("%.3f", hysteresisInputs[2]) ..
		"\n" ..
		" 4:" .. rowIOModesNamesLong[rowIOModes[4]] ..
		" 5:" .. rowIOModesNamesLong[rowIOModes[5]] ..
		" 6:" .. rowIOModesNamesLong[rowIOModes[6]] ..
		""
	)
end

function updateLights(block, row)
	-- Updates lights to display current status
	-- Use pilot lights to display output status.
	-- Use RGB { (red = 1), (green = 2), (blue = 3) }
	if voltsMaximum > epsilon and outputsAverage[row] > 0.0 then
		-- Positive Input, Use Green
		block.lights[row][1] = 0.0
		block.lights[row][2] = outputsAverage[row] / voltsMaximum
	elseif voltsMinimum < epsilon and outputsAverage[row] < 0.0 then
		-- Negative Input, Use Red
		block.lights[row][1] = outputsAverage[row] / voltsMinimum
		block.lights[row][2] = 0.0
	elseif outputsAverage[row] == 0.0 then
		block.lights[row][1] = 0.0
		block.lights[row][2] = 0.0
	else
		-- Something is wrong, turn both lights on...
		block.lights[row][1] = 1.0
		block.lights[row][2] = 1.0
	end
	
	-- Display in blue the difference from the last value to the current average.
	peakVal = clamp(math.abs(outputsLastVal[row] - outputsAverage[row]) / (math.abs(voltsMinimum) + voltsMaximum) * lightPeakGain, 0.0, 1.0)
	block.lights[row][3] = peakVal
	
	-- Update switch lights
	-- Use RGB { (red = 1), (green = 2), (blue = 3) }
	if rowIOModes[row] == rowModeMute then
		-- Muted, Channel off.
		-- Turn lights off
		block.switchLights[row][1] = 0.0
		block.switchLights[row][2] = 0.0
		block.switchLights[row][3] = 0.0
	elseif rowIOModes[row] == rowModeVolt then
		-- Voltage Offset Output mode.
		offsetVal = math.abs(offsetInputs[inputGroups[row]] - voltsMinimum) / (math.abs(voltsMinimum) + voltsMaximum)
		block.switchLights[row][1] = 1.0 - offsetVal
		block.switchLights[row][2] = offsetVal
		block.switchLights[row][3] = 0.0
	elseif rowIOModes[row] == rowModeDC then
		-- Playing, DC mode
		block.switchLights[row][2] = 0.5 + (peakVal * 0.5)
		block.switchLights[row][1] = peakVal
		block.switchLights[row][3] = 0.5 + (peakVal * 0.5)
	elseif rowIOModes[row] == rowModeAC then
		-- Playing, AC mode
		block.switchLights[row][1] = peakVal
		block.switchLights[row][2] = peakVal
		block.switchLights[row][3] = 0.5 + (peakVal * 0.5)
	else
		-- Error, unknown mode, animate lights.
		h = (1 - row / 6 + phase) % 1
		rgb = hsvToRgb(h, 1, 1)
		for c=1,3 do
			--block.lights[i][c] = rgb[c]
			block.switchLights[row][c] = rgb[c]
		end
	end
end

function readControls(block, row, lastCallTime)
	-- Check if inputs was initialised.
	if not (knobsInputFunctions[1]) then
		initInputs()
		return
	end
	
	-- Knob
	if math.abs(block.knobs[row] - knobsInputBuffer[row]) > epsilon then
		-- Knob moved
		-- Store value
		knobsInputBuffer[row] = block.knobs[row]
		-- Set display timer
		knobsDisplayTimer[row] = displayModeDelay
		
		-- Update Input
		knobFunc = knobsInputFunctions[row]
		if (knobFunc) then
			knobFunc(knobsInputBuffer[row])
		else
			-- Display error
			display("Error: Unknown knob function\n- Knob:" .. string.format("%s", row))
			-- Error display time
			displayUpdateTimer = 5.0
		end
	end
	
	-- Switch Debounce Timer
	if switchDisableTimer[row] > lastCallTime then
		-- Decrease debounce timer.
		switchDisableTimer[row] = switchDisableTimer[row] - lastCallTime
	elseif switchDisableTimer[row] > epsilon then
		-- Avoid going negative with the timer.
		switchDisableTimer[row] = 0.0
	end
	
	-- Check mute switch
	if block.switches[row] and switchDisableTimer[row] < epsilon then
		switchFunc = switchesInputFunctions[row]
		if (switchFunc) then
			switchFunc()
		else
			-- Display error
			display("Error: Unknown switch function\n- Switch:" .. string.format("%s", row))
			-- Error display time
			displayUpdateTimer = 5.0
		end
		-- Disable button for debouce time.
		switchDisableTimer[row] = debounceTime
		-- Set display timer for moddified switch.
		switchDisplayTimer[row] = displayModeDelay
	end
end

function processIO(block, row, lastCallTime)
	-- Get which group this input is part of
	inGroup = inputGroups[row]
	
	-- Get input gain
	gain = scaleInputs[inGroup]
	
	-- Check output mode
	if rowIOModes[row] == rowModeVolt or rowIOModes[row] == rowModeMute then
		-- Output constant voltage or disabled, set gain to zero
		gain = 0.0
	end
	
	outAvg = outputsAverage[row]
	offset = offsetInputs[inGroup]
	alphaLowPass = block.sampleTime / (1 - hysteresisInputs[inGroup] + block.sampleTime)
	hpRC = (math.pow(1 + hysteresisInputs[inGroup], hysteresisInputs[inGroup] * highpassGainCurve) - 1) / math.pow(2, highpassGainCurve)
	alphaHighPass = hpRC / (hpRC + block.sampleTime)
	--plot (((1+x)^(x*6))-1)/(2^6), x=0..1
	--alphaHighPass = hysteresisInputs[inGroup] / (hysteresisInputs[inGroup] + block.sampleTime)
	--alphaHighPass = hysteresisInputs[inGroup]
	
	if rowIOModes[row] == rowModeMute then
		-- Output disabled, set offset to zero
		offset = 0.0
	end
	
	-- Init values for filters
	lastOutVal = outputsLastVal[row]
	lastInVal = inputsLastVal[row]
	rMode = rowIOModes[row]
	-- Iterate input/output buffer
	for pos=1, block.bufferSize do
		-- Get input with gain
		inputValue = block.inputs[row][pos] * gain
		-- Calculate output
		outVal = 0.0
		if rMode == rowModeDC then
			--DC Mode, apply only low pass filter.
			outVal = lastOutVal + alphaLowPass * (inputValue - lastOutVal)
		elseif rMode == rowModeAC then
			--AC Mode, apply high pass filter.
			outVal = alphaHighPass * (lastOutVal + inputValue - lastInVal)
		end
		
		-- Set output with offset and clamp output.
		block.outputs[row][pos] = clamp(outVal + offset, voltsMinimum, voltsMaximum)
		
		--Updates values for next round.
		lastInVal = inputValue
		lastOutVal = outVal
		
		-- Reapply alpha to build inputAverage
		outAvg = outAvg + ((outVal - outAvg) * alphaLowPass)
	end
	
	inputsLastVal[row] = lastInVal
	outputsLastVal[row] = lastOutVal
	outputsAverage[row] = outAvg
end

function process(block)
	lastCallTime = block.sampleTime * config.frameDivider * config.bufferSize
	phase = phase + lastCallTime * 0.5
	phase = phase % 1
	
	-- Loop through each column
	for i=1,6 do
		-- Read Switches and Knobs
		readControls(block, i, lastCallTime)
		-- Process Inputs/Outputs
		processIO(block, i, lastCallTime)
		-- Update status lights
		updateLights(block, i)
	end
	-- Update text display
	updateDisplay(lastCallTime)
end

-- From https://en.wikipedia.org/wiki/HSL_and_HSV#HSV_to_RGB
function hsvToRgb(h, s, v)
	h = h * 6
	c = v * s
	x = c * (1 - math.abs(h % 2 - 1))
	if (h < 1) then rgb = {c, x, 0}
	elseif (h < 2) then rgb = {x, c, 0}
	elseif (h < 3) then rgb = {0, c, x}
	elseif (h < 4) then rgb = {0, x, c}
	elseif (h < 5) then rgb = {x, 0, c}
	else rgb = {c, 0, x}
	end
	m = v - c
	rgb[1] = rgb[1] + m
	rgb[2] = rgb[2] + m
	rgb[3] = rgb[3] + m
	return rgb
end

function ternary(cond, T, F)
	if cond then return T else return F end
end

function clamp(val, minv, maxv)
	if (val < minv) then
		return minv
	end
	if (val > maxv) then
		return maxv
	end
	return val
end

-- Return RC low-pass filter output samples, given input samples,
-- Time interval dt, Time constant rc
function lowpass(x, dt, rc)
	len = table.getn(x)
	y = {}
	table.setn(y, len)
	alpha = dt / (rc + dt)
	y[0] = alpha * x[0]
	for i=1, len do
		-- y[i] = alpha * x[i] + (1-alpha) * y[i-1]
		y[i] = y[i-1] + alpha * (x[i] - y[i-1])
	end
	return y
end

-- Return RC high-pass filter output samples, given input samples,
-- Time interval dt, and time constant RC
function highpass(x, dt, rc)
	len = table.getn(x)
	y = {}
	alpha = rc / (rc + dt)
	y[0] = x[0]
	for i=1, len do
		--y[i] = alpha * y[i-1] + alpha * (x[i] - x[i-1])
		y[i] = alpha * (y[i-1] + x[i] - x[i-1])
	end
	return y
end
