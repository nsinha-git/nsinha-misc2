from sortedcollections import SortedList, SortedDict
import pytest

car_speeds = [4,2,1,1]
car_pos = [0, 29, 35, 44]
state = 0
attempts = 0

def create_sorted_dist_lane(lane): #lane = list of car nos
    res = []
    for i in lane:
        res.append((i, car_pos[i], car_speeds[i]))
    return SortedList(res, key=lambda x: x[1])


lane_l = create_sorted_dist_lane([0,1,2,3])
lane_r = create_sorted_dist_lane([])
lanes = [lane_l, lane_r]


def find_min_coll_len(lane):
    m = 1e6
    for i in range(0, len(lane)):
        if i < len(lane) - 1:
            dist = lane[i + 1][1] - lane[i][1]
            if (lane[i][2] > lane[i + 1][2]):
                t = dist/(lane[i][2] - lane[i + 1][2])
                m = min(m,t)
    return m

def update_state():
    global state, attempts
    attempts += 1
    print(str(state) + "  attempts:" +  str(attempts))

    t = 1e6
    critical_lane = 0

    for (i,lane) in zip(range(0,len(car_speeds)),lanes):
        t1 = find_min_coll_len(lane)
        if (t > t1):
            t = t1
            critical_lane = i

    state += t

    for (i, lane) in zip(range(0, len(car_speeds)), lanes):
        lanes[i] = eval_locs(lane, t)

    car_tuple = remove_critical_car(lanes[critical_lane])
    col_happened = add_critical_car(lanes[critical_lane + 1 %2], car_tuple)

    if (col_happened):
        return state
    else:
        if all_sorted():
            return "Possible"
        else:
            update_state()



def eval_locs(lane, delta_t):
    n = SortedList(key=lambda x:x[1])

    for l in lane:
        ll = (l[0], l[1] + l[2]*delta_t, l[2])
        n.add(ll)
    return n

def remove_critical_car(lane):
    for i,l in zip(range(0, len(lane) - 1), lane):
        if round(l[1],2) == round(lane[i + 1][1],2):
            lane.remove(l)
            return l

def add_critical_car(lane, crit_car):
    if (crit_car is not None):
        lane.add(crit_car)

    for i,l in zip(range(0, len(lane)), lane):
        if (i != len(lane) - 1):
            if l[1] <= lane[i + 1][1] and l[1] > lane[i+1][1] -5 :
                return True
    return False

def all_sorted():
    global lanes
    for lane in lanes:
        last_speed = 0
        for x in lane:
            if (last_speed > x[2]):
                print (str(last_speed) + " cur_speed:" + str(x[2]))
                return False
            last_speed = x[2]
    return True


if __name__ == '__main__':
    update_state()

def test_each():
    find_min_coll_len(lane_l)
    p = eval_locs(lane_l, 6)
    c = remove_critical_car(p)
    add_critical_car(p, (1,40,2))
    add_critical_car(p, (1,41,2))
    assert False ==all_sorted()
