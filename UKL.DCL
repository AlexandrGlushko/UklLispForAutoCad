dc_ukl: dialog {label="�����";
:row{
	:column {
	:boxed_radio_column{label="����� � ����";
	 :text {label="����                  ��� ����     ����";}
	 :row {
			:popup_list{list="�������..."; width=12; key="ListLayer";}
			:edit_box {key="LayerName"; value="�����";}
			:image_button {width=4; key="LayerColor"; color=7;}
			}//row
		:text {label="����� ������                     ������";}
		:row {
			:popup_list {list=""; width=20; key="ListSText"; }
			:edit_box {edit_width=3; key="TextHeight"; value="0.5";}
				}//rwo
		
		}//boxed_radio_column
				:boxed_radio_row {label="��� �������";
							:radio_button {label=" <"; key="a1"; value="0";}
							:radio_button {label=" <---"; key="a2"; value="1";}
				}//boxed_row
	}//colimn
	:boxed_radio_column{label="��������";
		:radio_button {label="��"; 	key="g1"; value="1";}
		:radio_button {label="��"; 	key="g2"; value="0";}
		:radio_button {label="%"; 	key="g3"; value="0";}
		:radio_button {label="�"; 	key="g4"; value="0";}
		:radio_button {label="1:5"; key="g5"; value="0";}
	}//colimn
}//row
ok_cancel;
}//dialog