$(function() {
	$("#select_serie").change(function() {
		$.ajax({
	        method: "POST",
	        url: "/horaires",
	        data: {
	            action: "getSchedule",
	            serie: $("#select_serie").val()
	        }
	    }).done(function(resp) {
	        console.log(resp)
	    })
	 });
});
