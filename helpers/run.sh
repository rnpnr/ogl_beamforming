#!/bin/sh

base=/tmp/downloads
output=${base}

#studies="
#	250317_MN45-1_3.30MHz_ATS539_Cyst_TPW-128-TxRow
#	250317_MN45-1_3.30MHz_ATS539_Cyst_TPW-128-TxColumn
#	250317_MN45-1_3.30MHz_ATS539_Cyst_VLS-128-TxRow
#	250317_MN45-1_3.30MHz_ATS539_Cyst_VLS-128-TxColumn
#"

#axial="{5e-3, 120e-3}"
#lateral="{-30e-3, 30e-3}"
#
#while (true); do
#	for study in ${studies}; do
#		./helper --axial "${axial}" --lateral "${lateral}" ${study} ${base} ${base}/${study}*.zst
#		echo "showing: ${study}"
#		read _
#	done
#done

#./comp_paper --analytic --export ${output} ${base}
#./comp_paper --analytic --low-pass ${base}
#./comp_paper --low-pass ${base}
./comp_paper --analytic ${base}
