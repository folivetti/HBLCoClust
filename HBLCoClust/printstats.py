import pstats
f1 = open('hblcoclust.profile.txt','w')
stats = pstats.Stats('hblcoclust.profile',stream=f1)
stats.print_stats() 
f1.close()
