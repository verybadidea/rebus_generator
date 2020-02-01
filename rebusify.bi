sub rebusify(preStr as string, wordStr as string, postStr as string, emojiListStr() as string)
	dim as integer wordLen, iEmoji, emojiLen, mutatedLen, nothing = 1
	dim as string emojiStr, mutatedStr, partStr, rebusStr
	dim as string beforeStr, afterStr
	static as integer recursiveDepth = 0
	dim as string indent = string(recursiveDepth * 2, " ") 
	recursiveDepth += 1
	wordLen = len(wordStr)
	'colorPrint(indent & "1. find direct match", 14) 'TRAIN = TRAIN
	for iEmoji = 0 to ubound(emojiListStr)
		emojiStr = emojiListStr(iEmoji)
		if emojiStr = wordStr then
			rebusStr = lcase(emojiStr)
			'print indent & rebusStr
			rebusList.add(tokcat(preStr, rebusStr, postStr))
			nothing = 0
		end if
	next
	'colorPrint(indent & "2. find direct mutated match", 14) 'TRAIN -> TRAIL
	for iEmoji = 0 to ubound(emojiListStr)
		emojiStr = emojiListStr(iEmoji)
		for charPos as integer = 0 to len(emojiStr) - 1
			mutatedStr = emojiStr
			for char as integer = asc("A") to asc("Z")
				if char <> emojiStr[charPos] then
					mutatedStr[charPos] = char
					if mutatedStr = wordStr then
						rebusStr = lcase(emojiStr & " (" & chr(emojiStr[charPos]) & "=" & chr(char) & ")")
						'print indent & rebusStr
						rebusList.add(tokcat(preStr, rebusStr, postStr))
						nothing = 0
					end if
				end if
			next
		next
	next
	'colorPrint(indent & "3. find match with deleted char", 14) 'TRAIN -> TRAN
	for iEmoji = 0 to ubound(emojiListStr)
		emojiStr = emojiListStr(iEmoji)
		for charPos as integer = 0 to len(emojiStr) - 1
			mutatedStr = cutChar(emojiStr, charPos)
			if mutatedStr = wordStr then
				rebusStr = lcase(emojiStr & " (-" & chr(emojiStr[charPos]) & ")")
				'print indent & rebusStr
				rebusList.add(tokcat(preStr, rebusStr, postStr))
				nothing = 0
			end if
		next
	next
	'colorPrint(indent & "4. find partial direct match", 14) 'TRAIN + ING
	for iEmoji = 0 to ubound(emojiListStr)
		emojiStr = emojiListStr(iEmoji)
		emojiLen = len(emojiStr)
		if emojiLen < wordLen then
			for strPos as integer = 0 to wordLen - emojiLen
				partStr = mid(wordStr, strPos + 1, emojiLen)
				if partStr = emojiStr then
					rebusStr = lcase(emojiStr)
					beforeStr = mid(wordStr, 1, strPos)
					afterStr = mid(wordStr, strPos + emojiLen + 1)
					'print indent & tokcat(beforeStr, rebusStr, afterStr)
					'colorPrint(indent + simplecat(preStr, beforeStr, rebusStr, afterStr, postStr), 12)
					if len(beforeStr) >= 2 then
						rebusify(preStr, beforeStr, tokcat(rebusStr, afterStr, postStr), emojiListStr())
					end if
					if len(afterStr) >= 2 then
						rebusify(tokcat(preStr, beforeStr, rebusStr), afterStr, postStr, emojiListStr())
					end if
					if len(beforeStr) < 2 and len(afterStr) < 2 then
						rebusList.add(tokcat(preStr, beforeStr, rebusStr, afterStr, postStr))
					end if
					nothing = 0
				end if
			next
		end if
	next
	'colorPrint(indent & "5. find partial mutated match", 14) 'TRAM (M=I) + RING (R=N)
	for iEmoji = 0 to ubound(emojiListStr)
		emojiStr = emojiListStr(iEmoji)
		emojiLen = len(emojiStr)
		if emojiLen < wordLen then
			for strPos as integer = 0 to wordLen - emojiLen
				partStr = mid(wordStr, strPos + 1, emojiLen)
				for charPos as integer = 0 to len(emojiStr) - 1
					mutatedStr = emojiStr
					for char as integer = asc("A") to asc("Z")
						if char <> emojiStr[charPos] then
							mutatedStr[charPos] = char
							if mutatedStr = partStr then
								rebusStr = lcase(emojiStr & " (" & chr(emojiStr[charPos]) & "=" & chr(char) & ")")
								beforeStr = mid(wordStr, 1, strPos)
								afterStr = mid(wordStr, strPos + emojiLen + 1)
								'print indent & tokcat(beforeStr, rebusStr, afterStr)
								'colorPrint(indent + simplecat(preStr, beforeStr, rebusStr, afterStr, postStr), 13)
								if len(beforeStr) >= 2 then
									rebusify(preStr, beforeStr, tokcat(rebusStr, afterStr, postStr), emojiListStr())
								end if
								if len(afterStr) >= 2 then
									rebusify(tokcat(preStr, beforeStr, rebusStr), afterStr, postStr, emojiListStr())
								end if
								if len(beforeStr) < 2 and len(afterStr) < 2 then
									rebusList.add(tokcat(preStr, beforeStr, rebusStr, afterStr, postStr))
								end if
								nothing = 0
							end if
						end if
					next
				next
			next
		end if
	next
	'colorPrint(indent & "6. find partial match with deleted char", 14) 'T + BRAIN (-B) + ING
	for iEmoji = 0 to ubound(emojiListStr)
		emojiStr = emojiListStr(iEmoji)
		emojiLen = len(emojiStr)
		mutatedLen = emojiLen - 1
		for charPos as integer = 0 to emojiLen - 1
			mutatedStr = cutChar(emojiStr, charPos)
			if mutatedLen < wordLen then
				for strPos as integer = 0 to wordLen - mutatedLen
					partStr = mid(wordStr, strPos + 1, mutatedLen)
					if partStr = mutatedStr then
						beforeStr = mid(wordStr, 1, strPos)
						afterStr = mid(wordStr, strPos + mutatedLen + 1)
						'do not allow T + TRAIN (-T) or T + TRAIN (-T)
						'do not use F + OK (-K) + R use F + OK (K=R)
						if charPos = 0 andalso len(beforeStr) = 1 then continue for
						if charPos = emojiLen - 1 andalso len(afterStr) = 1 then continue for
						if charPos = 0 andalso right(beforeStr,1) = chr(emojiStr[charPos]) then continue for
						if emojiLen - 1 andalso left(afterStr,1) = chr(emojiStr[charPos]) then continue for
						rebusStr = lcase(emojiStr & " (-" & chr(emojiStr[charPos]) & ")")
						'print indent & tokcat(beforeStr, rebusStr, afterStr)
						'colorPrint(indent & simplecat(preStr, beforeStr, rebusStr, afterStr, postStr), 14)
						if len(beforeStr) >= 2 then
							rebusify(preStr, beforeStr, tokcat(rebusStr, afterStr, postStr), emojiListStr())
						end if
						if len(afterStr) >= 2 then
							rebusify(tokcat(preStr, beforeStr, rebusStr), afterStr, postStr, emojiListStr())
						end if
						if len(beforeStr) < 2 and len(afterStr) < 2 then
							rebusList.add(tokcat(preStr, beforeStr, rebusStr, afterStr, postStr))
						end if
						nothing = 0
					end if
				next
			end if
		next
	next
	if nothing = 1 then 'no further solutions found
		'print indent & simplecat(preStr, wordStr, postStr)
		rebusList.add(tokcat(preStr, wordStr, postStr))
	end if
	recursiveDepth -= 1
end sub
