;; Function to find blocks containing objects on a specific layer
;; Command name: FindBlocksWithLayer
;; Version 2: Replaced LOGTEST with LOGAND for compatibility
(defun c:FindBlocksWithLayer ( / *error* layerName blockTableEntry blockDefData blockName entity entityData entityLayer foundBlocks stopSearch blockFlags )

  ;; Local error handler
  (defun *error* ( msg )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    (princ) ; Exit quietly
  )

  ;; --- Main Function ---
  ;; Prompt the user for the layer name
  (setq layerName (getstring T "\nEnter layer name to search for inside blocks: "))

  ;; Check if the layer name is valid (not empty) and exists in the drawing
  (if (and layerName (/= layerName "") (tblsearch "LAYER" layerName))
    (progn
      (princ (strcat "\nSearching for blocks containing objects on layer: \"" layerName "\"..." ))
      (setq foundBlocks '() ) ; Initialize an empty list for found block names

      ;; Get the first entry in the block table
      (setq blockTableEntry (tblnext "BLOCK" T))

      ;; Loop through all block table entries
      (while blockTableEntry
        (setq blockName (cdr (assoc 2 blockTableEntry))) ; Get the block name
        (setq blockDefData (entget (tblobjname "BLOCK" blockName))) ; Get block definition data
        (setq blockFlags (cdr (assoc 70 blockTableEntry))) ; Get block flags (DXF code 70)

        ;; Process only regular user blocks (non-Xref, non-anonymous, non-layout)
        ;; Block definition flag 70 bits: 1=anonymous, 4=Xref, 64=Layout block (*Model_Space, *Paper_Space)
        ;; Check if NONE of these bits (1, 4, 64) are set using LOGAND
        ;; (= 0 (logand <bits_to_check> blockFlags)) means none of the bits are set
        (if (and blockDefData (= 0 (logand (+ 1 4 64) blockFlags)))
          (progn
            ;; Get the first entity within the block definition
            (setq entity (entnext (cdr (assoc -2 blockDefData))))
            (setq stopSearch nil) ; Flag to stop searching within this block once an entity is found

            ;; Loop through all entities within the current block definition
            (while (and entity (not stopSearch))
              (setq entityData (entget entity)) ; Get entity data
              (if entityData ; Ensure entity data is valid
                (progn
                  (setq entityLayer (cdr (assoc 8 entityData))) ; Get the entity's layer name (DXF code 8)
                  ;; Compare the entity's layer with the target layer (case-insensitive)
                  (if (and entityLayer (eq (strcase entityLayer) (strcase layerName)))
                    (progn
                      ;; Found an entity on the target layer
                      ;; Add the block name to the list if not already added (cons prepends)
                      (if (not (member blockName foundBlocks))
                          (setq foundBlocks (cons blockName foundBlocks))
                      )
                      (setq stopSearch T) ; Set flag to stop searching this block (optimization)
                    )
                  )
                )
              )
              (setq entity (entnext entity)) ; Move to the next entity in the block definition
            ) ; End while entity loop
          )
        ) ; End if blockDefData check

        (setq blockTableEntry (tblnext "BLOCK")) ; Move to the next block table entry
      ) ; End while blockTableEntry loop

      ;; --- Output the results ---
      (if foundBlocks
        (progn
          (princ (strcat "\n\nFound " (itoa (length foundBlocks)) " block(s) containing objects on layer '" layerName "':"))
          ;; Sort the list alphabetically (requires Visual LISP extensions, usually available)
          (foreach blkName (vl-sort foundBlocks '<) ; Sort alphabetically
            (princ (strcat "\n  - " blkName))
          )
        )
        ;; If no blocks were found
        (princ (strcat "\nNo user-defined blocks found containing objects on layer '" layerName "'."))
      )
    )
    ;; Else (layer does not exist or name was empty)
    (if (or (null layerName) (= layerName ""))
        (princ "\nOperation cancelled: No layer name entered.")
        (princ (strcat "\nError: Layer '" layerName "' not found in the drawing."))
    )
  ) ; End if layer exists check

  (princ) ; Suppress echoing the last evaluated expression (clean exit)
) ; End defun

;; --- Add a message indicating the command name upon loading ---
(princ "\nLISP script (v2 - using LOGAND) loaded. Type FINDBLOCKSWITHLAYER to run the search.")
(princ)