"""
Author: Nish Sinha
Licensed To: Peritus.ai

"""

import json
import sys
from kafka import KafkaProducer

print(sys.version)
producer = KafkaProducer(bootstrap_servers='kafka.int.peritus.ai:9092')


with open("/Users/nishchaysinha/cisco/UserStory-All-Fields-rally_200.json") as file:
    rally_data = json.load(file)

    for dict_line in rally_data:
        line_str = json.dumps(dict_line)
        print(line_str)
        producer.send('cisco_topic', line_str.encode('utf-8'))


#with open("/Users/nishchaysinha/cisco/UserStory-All-Fields-rally_200.json") as file:

producer.close()






