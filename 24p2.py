from sympy.matrices import Matrix

with open('input') as file:
    starts = []
    vels = []
    for _ in range(3):
        a, b = file.readline().split('@')
        a = list(int(i.strip()) for i in a.split(','))
        b = list(int(i.strip()) for i in b.split(','))
        starts.append(a)
        vels.append(b)

R = 300

for x in (range(-R, R)):
    for y in (range(-R, R)):
        bvel = [x, y]
        try:
            mtx = Matrix([
                [(bvel[0] - vels[0][0]), 0,                      1, 0],
                [(bvel[1] - vels[0][1]), 0,                      0, 1],
                [0,                      (bvel[0] - vels[1][0]), 1, 0],
                [0,                      (bvel[1] - vels[1][1]), 0, 1],
            ])
            s = mtx.LUsolve(Matrix([
                starts[0][0],
                starts[0][1],
                starts[1][0],
                starts[1][1],
            ]))
        except ValueError:
            pass
        else:
            if all(i == round(i) for i in s.values()):
                for z in range(-R, R):
                    bvel = [x, y, z]
                    try:
                        mtx = Matrix([
                            [(bvel[0] - vels[0][0]), 0,                      0,                      1, 0, 0],
                            [(bvel[1] - vels[0][1]), 0,                      0,                      0, 1, 0],
                            [(bvel[2] - vels[0][2]), 0,                      0,                      0, 0, 1],
                            [0,                      (bvel[0] - vels[1][0]), 0,                      1, 0, 0],
                            [0,                      (bvel[1] - vels[1][1]), 0,                      0, 1, 0],
                            [0,                      (bvel[2] - vels[1][2]), 0,                      0, 0, 1],
                            [0,                      0,                      (bvel[0] - vels[2][0]), 1, 0, 0],
                            [0,                      0,                      (bvel[1] - vels[2][1]), 0, 1, 0],
                            [0,                      0,                      (bvel[2] - vels[2][2]), 0, 0, 1],
                        ])
                        s = mtx.LUsolve(Matrix([
                            starts[0][0],
                            starts[0][1],
                            starts[0][2],
                            starts[1][0],
                            starts[1][1],
                            starts[1][2],
                            starts[2][0],
                            starts[2][1],
                            starts[2][2],
                        ]))
                    except ValueError as e:
                        pass
                    else:
                        vals = s.values()
                        if all(i == round(i) for i in s.values()):
                            print(vals[-1] + vals[-2] + vals[-3])
                            exit(0)
