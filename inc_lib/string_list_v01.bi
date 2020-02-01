#include "crt/stdlib.bi"

'-------------------------------------------------------------------------------

type string_list
	dim as string list(any)
	declare function size() as integer
	declare function empty() as integer
	declare function add(text as string) as integer
	declare function insert(text as string, position as integer) as integer
	declare function remove(position as integer) as integer
	'declare function find(text as string) as integer
	declare function clean() as integer 'remove doubles
	declare sub printAll() 
end type

function string_list.size() as integer
	return ubound(list) + 1
end function

function string_list.empty() as integer
	erase(list)
	return 0
end function

function string_list.add(text as string) as integer
	dim as integer ub = ubound(list)
	redim preserve list(ub + 1)
	list(ub + 1) = text
	return ub + 1
end function

function string_list.insert(text as string, position as integer) as integer
	dim as integer ub = ubound(list)
	if position < 0 then return -1
	if position > ub + 1 then return -1
	redim preserve list(ub + 1)
	for i as integer = ub to position step -1
		list(i + 1) = list(i) 'move down
	next
	list(position) = text 'insert
	return ub + 1
end function

function string_list.remove(position as integer) as integer
	dim as integer ub = ubound(list)
	if position < 0 then return -1
	if position > ub then return -1
	list(position) = list(ub) 'move last to remove position 
	redim preserve list(ub - 1)
	return ub - 1
end function

'remove doubles for list
function string_list.clean() as integer
	dim as integer i, j, numRemoved = 0
	while i <= ubound(list)
		j = i + 1
		while j <= ubound(list)
			if list(i) = list(j) then
				remove(j)
				numRemoved += 1
				j -= 1
			end if
			j += 1
		wend
		i += 1
	wend
	return numRemoved
end function

sub string_list.printAll()
	for i as integer = 0 to ubound(list)
		print i & " - " & list(i)
	next
end sub
