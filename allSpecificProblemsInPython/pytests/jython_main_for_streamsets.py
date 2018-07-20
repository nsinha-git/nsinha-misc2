import base64
for record in records:
    input_stream = record.value['fileRef'].getInputStream()
    newRecord = sdcFunctions.createRecord(record.sourceId)
    bytes = []
    while (True):
        cur_byte_in_int = input_stream.read()
        if (cur_byte_in_int == -1):
            break
        bytes.append(cur_byte_in_int)
    file_value = ''.join(map(chr, bytes))
    file_value_base64 = base64.b64encode(file_value)

    log.info(file_value)
    log.info(file_value_base64)


    filename = record.value['fileInfo']['filename']
    full_file_path_list = record.value['fileInfo']['file'].split("/")
    encoded_incident = full_file_path_list[len(full_file_path_list) - 2]
    (incident_key, project_key) = base64.decodestring(encoded_incident).split(",")
    sizeOfOriginalFile = record.value['fileInfo']['size']
    log.info(filename)
    newRecord.value = {'text' : file_value_base64 }
    newRecord.attributes['filename'] = filename
    newRecord.attributes['incidentKey'] = incident_key
    newRecord.attributes['projectKey'] = project_key
    output.write(newRecord)
    input_stream.close()





