with open("Lab1/input.txt", "r") as file_input:
    array = [int(x) for x in file_input.readlines()]
    print (str(sum(array))[:10])