# Alkiviadis Pavlou(2025930), Hui En Lin(8098735)
# PingPong Simulator v3.1.3
# Assignment B5
import logging
import numpy as np
from typing import Optional, Tuple
from PPData import * 
from itertools import accumulate, count

# Change to adapt the level of ouput from the python server:
# Values are DEBUG, INFO, ERROR
SPEED_CAP = 2.0
ACCEL_CAP = 5.0
LOGGING_LEVEL = logging.ERROR
EPS = 0.0001

NUMLINKS = 4

BAT_COLOR = "#242424"

LINK_COLORS = ["#242424", "#7f8086", "#7f8086", "#7f8086"]
LINK_LENGTHS = [0.3] * NUMLINKS

JOINT_COLORS = ["#fdff00", "#505c7c", "#242424", "#fdff00"]
JOINT_ANGLES = [-0.7, 1.2, 1.0, -0.5]

# Exercise 1
def name() -> str:
    return "Batman"


def make_arm() -> Arm:
    arm = Arm([], Bat(BAT_COLOR))

    for i in range(NUMLINKS):
        link = Link(LINK_COLORS[i], LINK_LENGTHS[i])
        joint = Joint(JOINT_COLORS[i], JOINT_ANGLES[i])
        arm.append(link, joint)

    return arm

# Exercise 3
EPS = 0.00000000001

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

def controlArm(time: Second, control: Control, arm: Arm) -> Arm:
    # Go through all joints and apply the acceleration
    applyControl(time, control, arm)
    
    # Move the angle for each joint accordingly
    advanceArm(time, arm)

    return arm

def applyControl(time: Second, control: Control, arm: Arm) -> Arm:
    for (_, joint), accel in zip(arm.comp, control.accelerations):
        controlJoint(time, accel, joint)

def controlJoint(time: Second, accel: RadianPerSquareSecond, joint: Joint):
    joint.jvel = capspeed(joint.jvel + capaccel(accel) * time)

def advanceArm(time: Second, arm: Arm):
    for _, joint in arm.comp:
        advanceJoint(time, joint)

def advanceJoint(time: Second, joint: Joint):
    joint.jang = joint.jang + joint.jvel * time

def evaluateArm(arm: Arm) -> List[Pnt]:
    # Create transformation matrices
    ts = makeGlobal(transformations(arm))

    # Find every point starting from (0,0) (as (0,0,1) in homogenous coordinates)
    vs = [t @ np.array([[0.0],[0.0],[1.0]]) for t in ts]

    # Remove doubled entries via dict.fromkeys and store as tuples of floats, also implicitly transforms from homogenous coordinates to 2D
    single_pnts = list(dict.fromkeys(map(lambda v: (v[0][0], v[1][0]), vs)))

    # Make points from the tuples
    pnts = [Pnt(p[0], p[1]) for p in single_pnts]

    return pnts

def makeGlobal(ts: np.array):
    gts = list(accumulate(ts, np.matmul, initial=identity()))
    return gts

def transformations(arm: Arm) -> np.array:
    ts = []
    for link, joint in arm.comp:
        tlink, rjoint = transformation(link, joint)
        ts.append(tlink)
        ts.append(rjoint)

    ts.append(transLink(arm.bat))

    return ts

def transformation(link: Link, joint: Joint) -> Tuple[np.array, np.array]:
    return transLink(link), rotJoint(joint)

def transLink(link: Link) -> np.array:
    return translation(Vec(0, link.llen))

def rotJoint(joint: Joint) -> np.array:
    return rotation(joint.jang)

def dance(time: Second, arm: Arm) -> Control:
    return [   20 * math.sin (6.0 * time)
           ,  -20 * math.cos (6.0 * time)
           ,   20 * math.sin (6.0 * time)
           ,  -20 * math.cos (6.0 * time)
           ]

# Exercise 5
# We use FABRIK to achieve inverse kinematics
def inverse(arm: Arm, seg: Seg) -> List[Radian]:
    # evaluateArm returns: base + joints + bat end
    jointPoses = [pntToArr(pnt) for pnt in evaluateArm(arm)]
    jointCount = len(jointPoses)
    # link lengthes + bat length
    linkLens = [comp[0].llen for comp in arm.comp]
    linkLens.append(arm.bat.llen)
    totalLength = sum(linkLens)
    # position of end effector and base
    base = np.array(jointPoses[0])
    p = pntToArr(seg.p)
    q = pntToArr(seg.q)

    # start point means the start position of controllable joint
    # since the first joint is fixed(always be [0, 0.1])
    # we should calculate the reachable point from that
    # reachable length should remove the fixed joint since it is not controllable,
    # so it can't be taken into account
    # we remove bat length because we calculate the distance from 
    # the position of end effector
    startPoint = base + np.array([0, 0.1])
    reachalbeLength = totalLength - arm.bat.llen - 0.1
    sp, sq = np.linalg.norm(p - startPoint), np.linalg.norm(q - startPoint)
    if reachalbeLength < (sp if sp > sq else sq):
        return None

    bs = p if sp > sq else q
    be = q if sp > sq else p

    targetDist = np.linalg.norm(jointPoses[jointCount - 2] - bs)

    iteration = 0
    
    while(targetDist > EPS):
        # forward reaching
        # set the bat align with segment
        # move prevJoint to new position by the length of
        # the link between joints
        # pj means previous joint, cj means current joint
        jointPoses[jointCount - 2] = bs
        jointPoses[jointCount - 1] = be
        
        # start with the index of last joint
        for index in range(jointCount - 3, -1, -1):
            pj = jointPoses[index]
            cj = jointPoses[index + 1]
            d = np.linalg.norm(pj - cj)
            weight = linkLens[index] / d
            jointPoses[index] = cj + ((pj - cj) * weight)
        
        # backward reaching
        # set the base joint align with its original position
        # move nextJoint to new position by the length of 
        # the link between joints
        # nj means next joint, cj means current joint
        jointPoses[0] = base
        jointPoses[1] = np.array([0, 0.1])

        for index in range(1, jointCount - 2):
            cj = jointPoses[index]
            nj = jointPoses[index + 1]
            d = np.linalg.norm(nj - cj)            
            weight = linkLens[index] / d
            # calculate the new position for nj
            jointPoses[index + 1] = cj + ((nj - cj) * weight)

        # update distEnd
        targetDist = np.linalg.norm(jointPoses[jointCount - 2] - bs)
        
        # counting iteration to prevent from an endless loop
        iteration += 1
        if iteration > 1000:
            break

    # calculate radians with positions
    radians = []
    prevRadian = np.pi / 2
    for j in range(1, jointCount - 1):
        vec = jointPoses[j + 1] - jointPoses[j]
        r = np.arctan2(vec[1], vec[0])
        r -= prevRadian
        radians.append(r)
        prevRadian += r

    return radians

def pntToArr(pnt: Pnt):
    return np.array([pnt.x, pnt.y])