'steps:
' find direct match -> done
' find mutated match -> done
' find match with deleted char -> done
' find partial direct match -> remainder
' find partial mutated match -> remainder
' find partial match with deleted char -> remainder
' remove doubles and sort
' remove lesser solutions
' choose one solution randomly 
' parse string to images
' repeat with next word in sentence

'todo:
' filter input string, allow "!" and "?"
' fix split.bi do allow empty results e.g. "a   " or " "
' add rebus wrap (next line)
' add more images (skate, sandal, trap, ...)
' change 2020 images to google version (rock, wood, helmet, bucket, feather, etc.)

'-------------------------------------------------------------------------------

const SCRN_W = 1280, SCRN_H = 600

#include "dir.bi"
#include "inc_lib/file_func_v01.bi"
#include "inc_lib/string_func_v01.bi"
#include "inc_lib/string_list_v01.bi"
#include "inc_lib/string_split_v01.bi"
#include "inc_lib/image_buffer_v01.bi"
image_draw_mode_default = IDM_ALPHA

dim shared as string_list rebusList
#include "rebusify.bi"

'sort by name
function qSortCallback1 cdecl(str1 as string, str2 as string) as long
	if str1 < str2 then return -1
	if str1 > str2 then return +1
	return 0
end function

'sort by capital count
function qSortCallback2 cdecl(str1 as string, str2 as string) as long
	dim as integer diff = countCapStr(str1) - countCapStr(str2)
	if diff < 0 then return -1
	if diff > 0 then return +1
	return 0
end function

sub sortListByName(list() as string)
	qsort(@list(0), ubound(list) + 1, sizeof(list), cptr(any ptr, @qSortCallback1))
end sub

sub sortListByCapi(list() as string)
	qsort(@list(0), ubound(list) + 1, sizeof(list), cptr(any ptr, @qSortCallback2))
end sub

'call after sorting the list
sub removeLesserSolutions(strList as string_list, margin as integer)
	dim as integer minChar = countCapStr(strList.list(0))
	dim as integer i = 0
	while i <= ubound(strList.list)
		if countCapStr(strList.list(i)) > minChar + margin then
			strList.remove(i)
			i -= 1
		end if
		i += 1
	wend
end sub

'-------------------------------------------------------------------------------

'form one list to the other, strip file dir and extension
sub imageBufferNamesToListStr(imgBuffer as image_buffer_type, listStr() as string)
	for i as integer = 0 to imgBuffer.numImages - 1
		dynamicAdd(listStr(), getFileNoExt(imgBuffer.imageFileName(i)))
	next
end sub

'convert result string to images
sub processRebusString(rubusStr as string, _
	emojiListStr() as string, emojiBuffer as image_buffer_type, _
	assistListStr() as string, assistBuffer as image_buffer_type)
	'remember x,y for next word
	static as integer x = 20, y = 80
	dim as integer iImage, xLow
	dim as int2d sizeEmoji = emojiBuffer.getImageSize(0) '90
	dim as int2d sizeAssist = assistBuffer.getImageSize(0) '30
	dim as integer yOffsetAssist = (sizeEmoji.y - sizeAssist.y) \ 2 '30
	dim as string splitStr(any)
	split(rubusStr, " ", splitStr())
	for i as integer = 0 to ubound(splitStr)
		'~ if i <> ubound(splitStr) then
			'~ print splitStr(i) & " | ";
		'~ else
			'~ print splitStr(i)
		'~ end if
		select case splitStr(i)[0] 'first char
		case asc("A") to asc("Z") 'FIXED CHARS
			for iChar as integer = 0 to len(splitStr(i)) - 1
				iImage = findInList(ucase(chr(splitStr(i)[iChar])), assistListStr())
				assistBuffer.drawImage(iImage, x, y + yOffsetAssist)
				x += sizeAssist.x
			next
		case asc("a") to asc("z") 'emoji
			iImage = findInList(ucase(splitStr(i)), emojiListStr())
			emojiBuffer.drawImage(iImage, x, y)
			x += sizeEmoji.x
		case asc("+") 'plus image
			iImage = findInList("PLUS", assistListStr())
			assistBuffer.drawImage(iImage, x, y + yOffsetAssist)
			x += sizeAssist.x
		case asc("(")
			select case splitStr(i)[1] 'second char
			case asc("-") 'minus image + char image
				xLow = x - (sizeEmoji.x - (sizeEmoji.x - sizeAssist.x * 2) \ 2) '-75
				iImage = findInList("MINUS", assistListStr())
				assistBuffer.drawImage(iImage, xLow, y + sizeEmoji.y)
				xLow += sizeAssist.x
				iImage = findInList(ucase(chr(splitStr(i)[2])), assistListStr())
				assistBuffer.drawImage(iImage, xLow, y + sizeEmoji.y)
			case else 'char image, equal image, char image
				xLow = x - (sizeEmoji.x - (sizeEmoji.x - sizeAssist.x * 3) \ 2) '-90
				iImage = findInList(ucase(chr(splitStr(i)[1])), assistListStr())
				assistBuffer.drawImage(iImage, xLow, y + sizeEmoji.y)
				xLow += sizeAssist.x
				iImage = findInList("EQUAL", assistListStr())
				assistBuffer.drawImage(iImage, xLow, y + sizeEmoji.y)
				xLow += sizeAssist.x
				iImage = findInList(ucase(chr(splitStr(i)[3])), assistListStr())
				assistBuffer.drawImage(iImage, xLow, y + sizeEmoji.y)
			end select
		end select
	next
	x += sizeAssist.x 'some space bewteen words
end sub

'-------------------------------------------------------------------------------

function main(sentenceStr as string) as string
	dim as string emojiListStr(any), assistListStr(any), wordListStr(any)
	dim as image_buffer_type emojiBuffer, assistBuffer
	
	if split(sentenceStr, " ", wordListStr()) = 0 then return "Error, invalid input string"
	print "Sentence to match: " & quote(sentenceStr)
	
	if emojiBuffer.loadDir("emoji_rebus_bmp/") <= 0 then return "Error, no rebus images found"
	imageBufferNamesToListStr(emojiBuffer, emojiListStr())
	'printList(emojiListStr()) : print "Num rebus images: " & ubound(emojiListStr) + 1

	if assistBuffer.loadDir("emoji_support_bmp/") <= 0 then return "Error, no support images found"
	imageBufferNamesToListStr(assistBuffer, assistListStr())
	'printList(assistListStr()) : print "Num support images: " & ubound(assistListStr) + 1

	'convert all to ucase
	ucaseList(emojiListStr())
	ucaseList(assistListStr())

	print "Processing";
	for i as integer = 0 to ubound(wordListStr)
		dim as string wordStr = ucase(wordListStr(i))
		print ".";
		rebusList.empty()
		rebusify("", wordStr, "", emojiListStr()) '<-- the ugly resursive rebus generator
		rebusList.clean()
		sortListByName(rebusList.list())
		sortListByCapi(rebusList.list())
		removeLesserSolutions(rebusList, 0)

		dim as integer iRebus = int(rnd * rebusList.size())
		'rebusList.printAll()
		processRebusString(rebusList.list(iRebus), emojiListStr(), emojiBuffer, assistListStr(), assistBuffer)
	next
	return ""
end function

'-------------------------------------------------------------------------------

screenres SCRN_W, SCRN_H, 32
width SCRN_W \ 8, SCRN_H \ 16
randomize timer
print main("freebasic rebus creator") '<-- word or sentence to change in to a rebus
print "Press any key to exit"
while inkey = "" : sleep 1 : wend
end
