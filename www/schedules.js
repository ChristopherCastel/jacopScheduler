$(function() {
	$("#select_serie").change(function() {
		var serieBloc = $("#select_serie").val().split("|")
		$.ajax({
	        method: "POST",
	        url: "/horaires",
	        data: {
	            action: "getSchedule",
	            serie: serieBloc[0],
	            bloc: serieBloc[1]
	        }
	    }).done(function(resp) {
	    	$("tbody").find("tr").each(function(i, tr) {
	    		$(this).find("td:gt(0)").each(function(j, td) {
	    			var slot_data = resp[i +  4 * j];
	    			if (slot_data[0] !== "vide") {
	    				$(this).text(slot_data[0] + " " + slot_data[1] + " " + slot_data[2]);
	    			} else {
	    				$(this).text("");
	    			}
	    		})
	    	})
	    })
	 });
});
