`(do0 HEAD
            ((Err message)
             (do0
              (if (dot (await (db--finish_failed (ref db) identifier message (ref now))) (is_ok)) (do0))))
            ((Ok gen)
             (do0
              (let
               ((end (dot (chrono--Utc--now) (to_rfc3339))))
               (if (dot (await (db--finish_success (ref db) identifier (dot gen summary (clone)) (dot gen in_tokens) (dot gen out_tokens) (ref end))) (is_ok)) (do0))
               (if-let
                ((Ok vec) (await (ai--embed_text (ref client) (ref api_key) (ref (dot gen summary)))))
                (do0
                 (if (dot (await (db--store_embedding (ref db) identifier (ai--embedding_to_bytes (ref vec)))) (is_ok)) (do0))))))))
          (do0)))))))
 ,TAIL)
