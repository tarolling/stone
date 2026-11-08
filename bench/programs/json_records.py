# json_records: writing a list of records as JSON text, reading it back, and walking what was read
import json
import sys


def records(n):
    items = []
    x = 12345
    for i in range(n):
        x = (x * 1103515245 + 12345) % 2147483648
        name = "user%d" % (x % 1000)
        if x % 7 == 0:
            name = 'say "hi"\n' + name
        tags = ["tag%d" % k for k in range(x % 4)]
        items.append({
            "id": i,
            "name": name,
            "score": float(x % 4000) / 4.0,
            "active": x % 3 == 0,
            "tags": tags,
            "pos": {"x": x % 640, "y": x // 640 % 480},
            "note": None,
        })
    return items


def checksum(v):
    if v is None:
        return 1
    if v is True:
        return 3
    if v is False:
        return 2
    if isinstance(v, int):
        return v
    if isinstance(v, float):
        return int(v * 4.0)
    if isinstance(v, str):
        return len(v.encode())
    if isinstance(v, list):
        total = 7
        for x in v:
            total += checksum(x)
        return total
    total = 11
    for k, x in v.items():
        total += len(k.encode()) + checksum(x)
    return total


def run(rounds, n):
    doc = records(n)
    total = 0
    for _ in range(rounds):
        text = json.dumps(doc, separators=(",", ":"), ensure_ascii=False)
        total += len(text.encode()) + checksum(json.loads(text))
    return total


print(run(int(sys.argv[1]), int(sys.argv[2])))
