(function (group) {
  if (group == "") {
    var groups = ["A", "A", "B", "B", "C"],
        rnd = BrightTag.Random.integer() % groups.length,
        date = new Date();
        date.setTime(date.getTime()+(365*86400000));
    var expires = date.toGMTString();
        
        document.cookie = "tag_group=" + groups[rnd] + ";domain=.surveymonkey.com;path=/;expires=" + expires;
        document.cookie = "BTgroup=;path=/;expires=Fri, 27 Jul 1999 02:47:11 UTC";

  }
}(bt_cookie("tag_group")))