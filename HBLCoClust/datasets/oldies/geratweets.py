f = open('tweets_unique.txt')
fw = open('tweets.data', 'w')

for idx, t in enumerate(f):
    for word in t.strip().split():
        if len(word) > 3 and 'http' not in word:
            fw.write('{}\t{}\n'.format(idx,word))


f.close()
fw.close()
