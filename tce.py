import math

x = [0] * 11
v = [0] * 11
s2 = [0] * 11
d = [0] * 11
c = [0] * 11
D = [0] * 11


dt = .01 
t = 0 
na = 11
for i in range(1, na):
	x[i]=0
	s2[i]=1

# velocidades
v[1] = 1.1
v[2] = 1.095
v[3] = 1.091
v[4] = 1.083
v[5] = 1.081
v[6] = 1.08
v[7] = 1.079
v[8] = 1.077
v[9] = 1.07
v[10] = 1.065

# diffusion coefficients
D[1] = .02
D[2] = .02
D[3] = .03
D[4] = .03
D[5] = .022
D[6] = .02
D[7] = .022
D[8] = .028
D[9] = .025
D[10] = .02

# concetration of each
c[1] = 10
c[2] = 5
c[3] = 5
c[4] = 4
c[5] = 1
c[6] = 5
c[7] = 5
c[8] = 8
c[9] = 8
c[10] = 7

# time of full round - fastest element
toroid_length_cm = 80
tau = toroid_length_cm / v[1]

num_round_trips = 10

m1 = 10 
m2 = 8
m3 = 6 
m4 = 4 
R1 = 0
R2 = 0
t=0

finished_fastest_analyte = False

for trip in range(1, 10):
	# termichal compresion of band "i" by a factor of 2 
	# happens when this condition is met
	t1 = tau / m1
	t2 = tau / m2
	t3 = tau / m3
	t4 = tau / m4
	# time inside loop
	tt = 0 
	t = t + dt
	tt = tt + dt 
	finished_fastest_analyte = False
	for i in range(1, na): # each analyte
		# print("going for " + str(t))
		y = x[1] # fastest
		x[i] = x[i] + v[i] * dt
		s2[i] = s2[i] + 2 * D[i] * dt
		# if x[i] > 35 or x[i] < 25:
		
		inside_t1 = tt > t1 and tt < t2
		inside_t2 = tt > t3 and tt < t4
		if inside_t1 or inside_t2:
			if y < 25 and x[i] > 25: 
				s2[i] = s2[i] / 2
			x[i] = x[i] - (v[i] * dt / 2)
			s2[i] = s2[i] + (2 * D[i] * dt / 10)

			
		if x[1] > 80:
			finished_fastest_analyte = True
		if x[i] > 80:
			x[i] = x[i] - 80
	if finished_fastest_analyte:
		R1 = 0
		R2 = 0 
		x[1] = x[1] + 80
		for i in range(1, na):
			print(x[i])
			print(s2[i])

		# resolutions among peaks 
		for i in range(na - 1):
			prevent_zero_division = 0.001
			R1 = R1 + (2 * (math.sqrt(s2[i]) + math.sqrt(s2[i + 1])) / (x[i] - x[i + 1] + prevent_zero_divisio))
			R2 = R2 + math.pow(((x[i] - x[i + 1]) / (2 * math.sqrt(x[i]) + 2 * math.sqrt(x[i + 1]))), 4)
			x[1] = x[1] - 80
		R1 = 9 / R1
		R2 = (R2 / 9) ^ (1 / 4)
		x[1] = x[1] - 80
	
	# print
	print("t: " + str(t) + " - R1:" + str(R1) + " - R2: " + str(R2))
	for i in range(1, 11):
		print(str(i) + " - x: " + str(x[i]) + " s2: " + str(s2[i]) + " c: " + str(c[i]) ) 
