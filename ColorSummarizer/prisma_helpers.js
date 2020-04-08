d3.json("prismas.json", function(error, prismas){
  d3.json("sushiopolis.json", function(error, sushi){

    prismaDict = {}
    prismas.forEach(function(color) {
      prismaDict[color.name] = color.color
    })
    sushi.forEach(function(color) {
      prismaDict[formatStr(color.name)] = color.color
    })

    var json = []
    Object.keys(prismaDict).forEach(function(key){
      json.push({"name": key, "color": prismaDict[key]})
    })
    pp(json)
  })
})
