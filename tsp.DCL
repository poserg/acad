tsp: dialog{label="Draw tsp";
	:edit_box{ label="Пролет, k"; key="k"; value="24"; edit_width="6";}
	:column {label="Шаг колонн";
		:edit_box {label="Bкр"; key="Bkr"; value="6"; edit_width="6";}
		:edit_box {label="Bср"; key="Bsr"; value="12"; edit_width="6";}
		:edit_box {label="Bф"; key="Bf"; value="6"; edit_width="6";}
	} // column
	:edit_box {label="Число пролетов, m"; key="m"; value="3"; edit_width="6";}
	:edit_box {label="Длина здания, L"; key="L"; value="72"; edit_width="6";}
	ok_cancel;
} // dialog