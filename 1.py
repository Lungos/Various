with open("data1.txt", "r") as textfile:
	code = textfile.read()

di = 0
pos = [0, 0]

dic = {}
dic[(pos[0], pos[1])] = True

walks = str.split(code, ", ")
foundFirst = False

def distance(value):
	total = 0
	for val in value:
		total += abs(val)
	return str(total)

def walk(di):
	if di == 0:
		pos[1] += 1
	elif di == 1:
		pos[0] += 1
	elif di == 2:
		pos[1] -= 1
	else:
		pos[0] -= 1

for step in walks:
	turn = step[0]
	length = int(step[1:])

	if turn == 'R':
		di = (di + 1) % 4
	else:
		di = (di - 1) % 4
	for i in range(length):
		walk(di)
		if not foundFirst:
			if (pos[0], pos[1]) in dic:
				foundFirst = True
				answerB = distance(pos)
			else:
				dic[(pos[0], pos[1])] = True

print "Answer for 1A: " + distance(pos)
print "Answer for 1B: " + answerB
