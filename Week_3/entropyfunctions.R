# This is the location of my own test data set (entropy-test-file.csv)
# Will have to change it to the users file location

dataset = read.csv("C:/Users/Jare/SkyDrive/WorkDocs/CUNY/607/Week 3/entropy-test-file.csv", header=TRUE)


entropy = function (d){
    # takes a vector d, and returns the entropy
    # will use the table function to build a contigency table and store the number of observations in n
    # the contigency table will be stored in p, while the probabilities in p1 and p2 (count of p1)/total
    # number of
    # observations

    p = table(d)
    p1 = p[[1]]/length(d)
    p2 = p[[2]]/length(d)

    # will have to take care of the situation where p1 or p2 = 0 (0log2(0) = inf), and make it 0

    if ((p1 == 0) || (p2==0)){

        return (1)

    } else
    {
        return (-(p1*log2(p1) + p2*log2(p2)))
    }
}


infogain = function (d, a){
    v = vector()
    nv = vector()
    w = data.frame(a, d)
    numObs = nrow(w)
    w[1] = as.factor(w[,1])
    f = nlevels(as.factor(w[,1]))
    for (i in 1:f){

        t = subset(w, w[,1]==levels(w[,1])[i])
        v = c(v, entropy(t[,2]))
        nv = c(nv, nrow(t))
    }

    # sum all the values of the entropy by the count of the partitions
    s = 0
    for (i in 1:length(v)){
        s = s + (nv[i]/numObs)*v[i]
    }

    return(entropy(d)- s)

}


# Create a function decide() that takes a data frame - the target and the collection
# of candidate attributes  with which to partition the data - and the number of
# the column that is the target and returns a list containing two items: the identity
# (by column number) of the attribute that maximizes the information gain and a vector
# of the information gains for each of the candidate attributes.

decide = function(dataset, n){

    v = 0
    y = 1
    s = 0
    for (x in 1:ncol(dataset)){
        if (x != n){

            v[y] = infogain(dataset[,n],dataset[,x])
            s[y] = names(dataset[x])
            y= y + 1
        }
    }


    names(v) = s

    #print (list(c(max = max(v)), (gain = v)))
    # create a list to hold the answers
    ls = list()

    # will create a sort vector from our vector results v, which will be in descreasing order to show the max gain

    w = sort(v, decreasing = TRUE)

    # The first element of the vector will containg the highest gain value
    # will extract the name of the column it corresponds to, and then extract the column in belongs to from the dataset

    colNum = match(names(w[1]), names(dataset))

    ls$max  = colNum
    ls$gains = v      # add the answers to a list named gains

    return (ls)


}


entropy(dataset$answer)
infogain(dataset$answer, dataset$attr1)
infogain(dataset$answer, dataset$attr2)
infogain(dataset$answer, dataset$attr3)

decide(dataset, 4)




