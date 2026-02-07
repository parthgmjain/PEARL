(gates lines) -> (gates lines) with (gate gatesRev ls l1 l2 l3 v1 v2 v3)

init:
	from act1
	exit

act1:
	fi (gate = 'CCNOT)
		from toff
		else cnot
	gates <- ((gate . ls) . gates)
	if gatesRev
		goto act2
		else init

toff:
	from act2
	(l1 . (l2 . (l3 . 'nil))) <- ls
	v1 <- lines[l1]
	v2 <- lines[l2]
	v3 <- lines[l3]
	v3 ^= (v1 && v2)
	lines[l3] <- v3
	lines[l2] <- v2
	lines[l1] <- v1
	ls <- (l1 . (l2 . (l3 . 'nil)))
	goto act1

cnot:
	from act2
	(l1 . (l2 . 'nil)) <- ls
	v1 <- lines[l1]
	v2 <- lines[l2]
	v2 ^= v1
	lines[l2] <- v2
	lines[l1] <- v1
	ls <- (l1 . (l2 . 'nil))
	goto act1

act2:
	fi gates
		from act1
		else act3
	((gate . ls) . gatesRev) <- gatesRev
	if (gate = 'CCNOT)
		goto toff
		else cnot

act3:
	fi gatesRev
		from act3
		else stop
	(gate . gates) <- gates
	gatesRev <- (gate . gatesRev)
	if gates
		goto act3
		else act2

stop:
	entry
	goto act3
