# Alkiviadis Pavlou(2025930), Hui En Lin(8098735)
# PingPong Simulator v3.1.5
# Assignment B7
from PPData import *
from typing import Optional, List, Tuple, Callable
import logging
import numpy as np
import math
from itertools import accumulate
import copy
import operator

GRAVITY = 2.0
SPEED_CAP = 2.0
ACCEL_CAP = 5.0
# Change to adapt the level of ouput from the python server:
# Values are DEBUG, INFO, ERROR
LOGGING_LEVEL = logging.ERROR
EPS = 0.0001

NUMLINKS     = 4

BAT_COLOR    = "#341235"

LINK_COLORS  = ["#a111b2"]  *  NUMLINKS
LINK_LENGTHS = [0.3, 0.3, 0.3, 0.3]

JOINT_COLORS = ["#777777"] * NUMLINKS
JOINT_ANGLES = [-0.7, 1.2, 1.0, -0.5]

def almostZero(number: float) -> bool:
    if abs(number) < EPS:
        return True
    return False

def cap(real: float, capval: float) -> float:
    realabs = abs(real)
    if almostZero(realabs - capval) or realabs > capval:
        if real < 0:
            return -capval
        return capval

    return real

def capspeed(speed: float) -> float:
    return cap(speed, SPEED_CAP)

def capaccel(accel: float) -> float:
    return cap(accel, ACCEL_CAP)

#Exercise 1
def name() -> str:
    return "pyping"

def make_arm() -> Arm:
    arm = Arm([], Bat(BAT_COLOR))

    for i in range(NUMLINKS):
        link = Link(LINK_COLORS[i], LINK_LENGTHS[i])
        joint = Joint(JOINT_COLORS[i], JOINT_ANGLES[i])
        arm.append(link, joint)

    return arm

#Exercise 2
def detectCollision(snap1: Snapshot, snap2: Snapshot) -> Optional[Second]:
    pocos = potentialCollisions(snap1, snap2)
    time = None
    for poco in pocos:
        if validPoco(poco):
            time = collisionTime(snap1, snap2, poco[0])
            break

    return time

def between_zero_and_one(x: float):
    return x >= 0 and x <= 1

def validPoco(poco: List[float]):
    return between_zero_and_one(poco[0]) and between_zero_and_one(poco[1])

def collisionTime(snap1: Snapshot, snap2: Snapshot, t: float) -> Second:
    return (1 - t) * snap1.time + t * snap2.time

def computeSegParameter(xa: float, xb: float, xc: float, xd: float, ya: float, yb: float, yc:float, yd: float, t: float) -> Optional[float]:
    f = 0.0
    x_zero = almostZero(xa + xc * t)
    y_zero = almostZero(ya + yc * t)
    if x_zero and y_zero:
        f = None
    elif x_zero:
        f = (yd - yb * t) / (ya + yc * t)
    elif y_zero:
        f = (xd - xb * t) / (xa + xc * t)
    else:
        f1 = (xd - xb * t) / (xa + xc * t)
        f2 = (yd - yb * t) / (ya + yc * t)
        if between_zero_and_one(f1):
            f = f1
        else:
            f = f2

    return f

def potentialCollisions(snap1: Snapshot, snap2: Snapshot) -> List[List[float]]:
    xb0, yb0 = snap1.ball.x, snap1.ball.y
    xb1, yb1 = snap2.ball.x, snap2.ball.y
    
    c0, d0 = snap1.segment.p, snap1.segment.q
    xc0, yc0 = c0.x, c0.y
    xd0, yd0 = d0.x, d0.y

    c1, d1 = snap2.segment.p, snap2.segment.q
    xc1, yc1 = c1.x, c1.y
    xd1, yd1 = d1.x, d1.y

    xa = xd0 - xc0
    ya = yd0 - yc0
    xb = xb0 - xc0 + xc1 - xb1
    yb = yb0 - yc0 + yc1 - yb1 
    xc = xc0 - xd0 + xd1 - xc1
    yc = yc0 - yd0 + yd1 - yc1
    xd = xb0 - xc0
    yd = yb0 - yc0

    i = xd * ya - yd * xa
    j = xd * yc - xb * ya - yd * xc + yb * xa
    k = yb * xc - xb * yc

    ts = solve_quadratic_eq(k, j, i)
    fs = []
    for t in ts:
        f = computeSegParameter(xa, xb, xc, xd, ya, yb, yc, yd, t)
        if f != None:
            fs.append(f) 

    # print(sorted(zip(ts, fs))) 
    return sorted(zip(ts, fs))


def solve_azero(a: float, b: float, c: float) -> List[float]:
    return [-c / b]

def solve_bzero(a: float, b: float, c: float) -> List[float]:
    val = -c / a
    val_sign = math.copysign(1.0, val)
    if val_sign < 0:
        return []
    else:
        return [math.sqrt(val)]

def solve_czero(a: float, b: float, c: float) -> List[float]:
    return sorted([0, -b / a])

def solve_nonzero(a: float, b: float, c: float) -> List[float]:
    d = (b * b) - 4 * a * c
    d_sign = math.copysign(1.0, d)

    if almostZero(d):
        return [-b / (2 * a)]
    elif d > 0:
        return [((-b) - math.sqrt(d)) / (2 * a), ((-b) + math.sqrt(d)) / (2 * a)]
    else:
        return []

def solve_quadratic_eq(a: float, b: float, c: float) -> List[float]:
    res = []
    a_zero = almostZero(a)
    b_zero = almostZero(b)
    c_zero = almostZero(c)
    
    if c_zero and (a_zero or b_zero):
        res = [0] 
    elif a_zero and b_zero:
        res = []
    elif c_zero:
        res = solve_czero(a, b, c) 
    elif b_zero:
        res = solve_bzero(a, b, c)
    elif a_zero:
        res = solve_azero(a, b, c)
    elif almostZero(a / b) or almostZero(a / c):
        res = solve_quadratic_eq(0.0, b, c)
    else:
        res = solve_nonzero(a, b, c)

    return res

#Exercise 3
def controlArm(time: Second, control: Control, arm: Arm) -> Arm:
    apply_control(time, control, arm)
    advance_arm(time, arm)

    return arm

def apply_control(time: Second, control: Control, arm: Arm) -> Arm:
    for (_, joint), accel in zip(arm.comp, control.accelerations):
        control_joint(time, accel, joint)

def control_joint(time: Second, accel: RadianPerSquareSecond, joint: Joint):
    print(time, joint.jvel + capaccel(accel) * time)
    joint.jvel = capspeed(joint.jvel + capaccel(accel) * time)

def advance_arm(time: Second, arm: Arm):
    for _, joint in arm.comp:
        advance_joint(time, joint)

def advance_joint(time: Second, joint: Joint):
    joint.jang = joint.jang + joint.jvel * time

def evaluateArm(arm: Arm) -> List[Pnt]:
    ts = make_global(transformations(arm))
    vs = [t @ np.array([[0.0],[0.0],[1.0]]) for t in ts]
    vecs = [Vec(v[0][0], v[1][0]) for v in vs]
    pnts = [origin + vec for vec in vecs]
    single_pnts = list(dict.fromkeys(map(lambda p: (p.x,p.y), pnts)))
    pnts = [Pnt(p[0], p[1]) for p in single_pnts]

    return pnts

def make_global(ts: np.array):
    gts = list(accumulate(ts, np.matmul, initial=identity()))
    return gts

def transformations(arm: Arm) -> np.array:
    ts = []
    for link, joint in arm.comp:
        tlink, rjoint = transformation(link, joint)
        ts.append(tlink)
        ts.append(rjoint)

    ts.append(trans_link(arm.bat))

    return ts

def transformation(link: Link, joint: Joint) -> Tuple[np.array, np.array]:
    return trans_link(link), rot_joint(joint)

def trans_link(link: Link) -> np.array:
    return translation(Vec(0, link.llen))

def rot_joint(joint: Joint) -> np.array:
    return rotation(joint.jang)

def dance(time: Second, arm: Arm) -> Control:
    return [ -2 * math.sin(2.2 * time)
            , -2 * math.cos(2.3 * time)
            ,  2 * math.sin(2.4 * time)
            ,  2 * math.cos(2.5 * time)
            ]

#Exercise 4
def handleCollision(snap1: Snapshot, snap2: Snapshot, time: Second) -> Tuple[Pnt, Vec]:
    collision, f = collisionPoint(snap1, snap2, time)
    v = collisionVelocity(snap1, snap2, time, f)
    
    return (collision, v)

def getTFromTime(time: float, time0: float, time1: float):
    return (time - time0) / (time1 - time0)

def collisionPoint(snap1: Snapshot, snap2: Snapshot, time: Second) -> Tuple[Pnt, float]:
    xb0, yb0 = snap1.ball.x, snap1.ball.y
    xb1, yb1 = snap2.ball.x, snap2.ball.y
    
    c0, d0 = snap1.segment.p, snap1.segment.q
    xc0, yc0 = c0.x, c0.y
    xd0, yd0 = d0.x, d0.y

    c1, d1 = snap2.segment.p, snap2.segment.q
    xc1, yc1 = c1.x, c1.y
    xd1, yd1 = d1.x, d1.y

    xa = xd0 - xc0
    ya = yd0 - yc0
    xb = xb0 - xc0 + xc1 - xb1
    yb = yb0 - yc0 + yc1 - yb1 
    xc = xc0 - xd0 + xd1 - xc1
    yc = yc0 - yd0 + yd1 - yc1
    xd = xb0 - xc0
    yd = yb0 - yc0

    t = getTFromTime(time, snap1.time, snap2.time)
    f = computeSegParameter(xa, xb, xc, xd, ya, yb, yc, yd, t)
    p = origin + (1 - t) * (1 - f) * (c0 - origin) \
               + (1 - t) * f       * (d0 - origin) \
               + t       * (1 - f) * (c1 - origin) \
               + t       * f       * (d1 - origin)

    return (p, f)
        
def reflectVector(v, d):    
    a = angle(Vec(1, 0), d)
    transform = rotation(a) @ reflectvertical() @ rotation(-a)
    np_v = np.array([v.a, v.b, 0])
    reflected = np_v @ transform
    return Vec(reflected[0], reflected[1])


def collisionVelocity(snap1, snap2, time, f) -> Vec:
    t = getTFromTime(time, snap1.time, snap2.time)
    c0 = snap1.segment.p
    d0 = snap1.segment.q
    c1 = snap2.segment.p
    d1 = snap2.segment.q
    tdiff = snap2.time - snap1.time

    v_line =   ((1 - t) * (d0 - origin) + t * (d1 - origin)) \
             - ((1 - t) * (c0 - origin) + t * (c1 - origin))

    v_seg =   (((1 - f) * (c1 - origin) + f * (d1 - origin)) \
            - ((1 - f) * (c0 - origin) + f * (d0 - origin))) \
            / tdiff

    v_ball = (snap2.ball - snap1.ball) / tdiff
    v_diff = v_ball - v_seg
    v_ref = reflectVector(v_diff, v_line)

    res = v_ref + v_seg
    return res

#Exercise 5
ITERATIONS = 1000
from itertools import cycle

def angles(arm: Arm) -> List[Radian]:
    return [j.jang for _, j in arm.comp]

def inverse(arm: Arm, seg: Seg) -> Optional[List[Radian]]:
    arm, possible = solveInverse(ITERATIONS, arm, seg)

    if possible:
        return angles(arm)
    else:
        arm, possible = solveInverse(ITERATIONS, arm, Seg(seg.q, seg.p))
        if possible:
            return angles(arm)
    
    return None

def solveInverse(iterations: int, arm: Arm, seg: Seg) -> Tuple[Arm, bool]:
    arm, possible = ccd(iterations, seg.p, arm)
    setFinalAngle(arm, seg.q)

    return (arm, possible)

def alength(arm: Arm):
    return len(arm.comp)

def ithjoint(i: int, arm: Arm) -> Joint:
    if i - 1 >= 0 and i - 1 < alength(arm):
        return arm.comp[i - 1][1]
    
    raise IndexError("Not enough joints.")

def addRadian(s: Radian, t: Radian) -> Radian: 
    return (s + t) % (2 * math.pi)

def ccd(iterations: int, goal: Pnt, arm: Arm) -> Tuple[Arm, bool]:
    round = 0
    for i in cycle(range(1, alength(arm) + 1)):
        joint = ithjoint(i, arm)
        positions = evaluateArm(arm)
        pivot = positions[-2]

        distance = (goal - pivot).norm()
        if distance < EPS:
            break
        elif round == iterations:
            return (arm, False)

        step(joint, positions[i], pivot, goal)

        round = round + 1
    
    return (arm, True)
        
def step(joint: Joint, jpos: Pnt, pivot: Pnt, goal: Pnt):
    diff = angle((pivot - jpos), (goal - jpos))
    frac = 0.999 * diff
    joint.jang = addRadian(joint.jang, frac)

def setFinalAngle(arm: Arm, goal: Pnt):
    positions = evaluateArm(arm)
    fjoint = ithjoint(alength(arm), arm)
    tip = positions[-1]
    piv = positions[-2]
    u = tip - piv
    v = goal - piv
    dif = angle(u, v)

    fjoint.jang = addRadian(fjoint.jang, dif)

#Exercise 6
def rotate90(vec: Vec) -> Vec:
    return Vec(-vec.b, vec.a)

def computeVector(p: Pnt, q: Pnt) -> Vec:
    return rotate90(p - q)

def iterativeLinearCombination(iterations, goal, vectors, current):
    round = 0
    while round < iterations:
        now = linearCombination(current, vectors)
        dif = goal - now

        if almostZero(dif.a) and almostZero(dif.b):
            return current

        current = update(dif, vectors, current)

        round += 1

    return current

def update(dif, vectors, current):
    new = []
    for v, c in zip(vectors, current):
        new.append(updateCoefficient(dif, v, c))

    return new

def updateCoefficient(dif, v, c):
    if almostZero(v.a) and almostZero(v.b):
        return c

    d = project(dif, v)
    x = d - c 
    return capspeed(c + 0.3 * x)

def project(u: Vec, v: Vec) -> float:
    return u * (v / v.norm())

def linearCombination(current: List[float], vectors: List[Vec]) -> Vec:
    mults = [c * v for c, v in zip(current, vectors)]
    return sum(mults, Vec(0,0))

def inverseVelocityKinematics(arm, goal):
    points = evaluateArm(arm)[1:-1]
    pivot = points[-1]
    vectors = list(map(lambda p: computeVector(pivot, p), points))
    current = [j.jvel for _, j in arm.comp]
    s = iterativeLinearCombination(10, goal, vectors, current)
    s[-1] = -sum(s[:-1])

    for (_, joint), si in zip(arm.comp, s):
        joint.jvel = si

    return arm

def plan(current_time: Second, arm: Arm, time_bound: Second, 
            seg: Seg, velocity: Vec) -> Control:

    current_arm = copy.deepcopy(arm)
    goalArm, possible = solveInverse(10, current_arm, seg)
    inverseVelocityKinematics(goalArm, velocity)

    span = time_bound - current_time
    control = []
    for (_, cjoint) , (_, gjoint) in zip(arm.comp, goalArm.comp):
        acc = planJoint(span, cjoint, gjoint)
        control.append(acc)

    return control

def modAngle(a):
    return ((a + math.pi) % (2 * math.pi)) - math.pi

def planJoint(span: Second, cjoint: Joint, gjoint: Joint) -> RadianPerSquareSecond:
    current = DataPoint(0.0, modAngle(cjoint.jang), cjoint.jvel)
    goal = DataPoint(span, modAngle(gjoint.jang), gjoint.jvel)
    res = fitCubic(current, goal)

    return 2 * res[1]

@dataclass
class DataPoint:
    time: Second
    angle: Radian
    velocity: RadianPerSecond

def fitCubic(p0: DataPoint, p1: DataPoint):
    m = np.array([
        [    p0.time ** 3,     p0.time ** 2, p0.time, 1],
        [    p1.time ** 3,     p1.time ** 2, p1.time, 1],
        [3 * p0.time ** 2, 2 * p0.time     ,       1, 0],
        [3 * p1.time ** 2, 2 * p1.time     ,       1, 0]
    ])
    r = np.array([p0.angle, p1.angle, p0.velocity, p1.velocity])

    if np.linalg.det(m) == 0:
        return [0, 0, 0, 0]
    
    return np.linalg.inv(m) @ r

#Exercise 7
def action(time: Second, item: Item, arm: Arm, ball: BallState) -> Control:
    goalTime = 8.7
    goalSeg = Seg(Pnt(-0.3, 0.7), Pnt(-0.3, 0.8))
    goalVel = Vec(-1, 0)

    return plan(time, arm, goalTime, goalSeg, goalVel)