(ns hive-schemas.git
  "Git area value objects shared across addons."
  (:require [hive-spi.schema.registry :as reg]))

(def Area [:enum :head :index :worktree :stash :stage/base :stage/ours :stage/theirs])
(def AreaRef [:map [:area/kind Area] [:area/ref {:optional true} :string]])
(def LineRange [:map [:start nat-int?] [:count nat-int?]])
(def LineOp [:enum :ctx :add :del])
(def Line [:tuple LineOp :string])
(def Hunk [:map [:hunk/old LineRange] [:hunk/new LineRange]
           [:hunk/header {:optional true} :string] [:hunk/lines [:vector Line]]
           [:hunk/forms {:optional true} [:vector :string]]])
(def ChangeStatus [:enum :added :modified :deleted :renamed :copied :type-changed
                   :unmerged :untracked :ignored])
(def FileChange [:map [:change/status ChangeStatus] [:change/path :string]
                 [:change/from {:optional true} :string]
                 [:change/similarity {:optional true} [:int {:min 0 :max 100}]]
                 [:change/mode {:optional true} [:map [:old {:optional true} :string]
                                                [:new {:optional true} :string]]]
                 [:change/binary? {:optional true} :boolean]
                 [:change/hunks {:optional true} [:vector Hunk]]])
(def AreaDiff [:map [:diff/repo :string] [:diff/from AreaRef] [:diff/to AreaRef]
               [:diff/changes [:vector FileChange]]])
(def Blob [:map [:blob/sha {:optional true} :string] [:blob/text {:optional true} :string]])
(def ConflictKind [:enum :both-modified :both-added :both-deleted :added-by-us
                   :added-by-them :deleted-by-us :deleted-by-them])
(def ConflictRegion [:map [:region/ours LineRange] [:region/theirs LineRange]
                     [:region/base {:optional true} LineRange]
                     [:region/markers LineRange]
                     [:region/ours-lines [:vector :string]]
                     [:region/theirs-lines [:vector :string]]
                     [:region/base-lines {:optional true} [:vector :string]]])
(def Conflict [:map [:conflict/path :string] [:conflict/kind ConflictKind]
               [:conflict/base {:optional true} Blob]
               [:conflict/ours {:optional true} Blob]
               [:conflict/theirs {:optional true} Blob]
               [:conflict/regions {:optional true} [:vector ConflictRegion]]])
(def Stash [:map [:stash/index nat-int?] [:stash/ref :string] [:stash/message :string]])
(def Operation [:enum :merge :rebase :cherry-pick :revert :bisect :am])
(def Status [:map [:status/repo :string]
             [:status/branch {:optional true} [:maybe :string]]
             [:status/detached? :boolean]
             [:status/head {:optional true} [:maybe :string]]
             [:status/upstream {:optional true} :string]
             [:status/ahead {:optional true} nat-int?]
             [:status/behind {:optional true} nat-int?]
             [:status/operation {:optional true} Operation]
             [:status/staged [:vector FileChange]]
             [:status/unstaged [:vector FileChange]]
             [:status/untracked [:vector :string]]
             [:status/conflicts [:vector Conflict]]
             [:status/stashes [:vector Stash]]])

(reg/register-all! {:hive.schemas.git/area Area :hive.schemas.git/area-ref AreaRef
                    :hive.schemas.git/line-range LineRange :hive.schemas.git/line-op LineOp
                    :hive.schemas.git/line Line :hive.schemas.git/hunk Hunk
                    :hive.schemas.git/change-status ChangeStatus
                    :hive.schemas.git/file-change FileChange :hive.schemas.git/area-diff AreaDiff
                    :hive.schemas.git/blob Blob :hive.schemas.git/conflict-kind ConflictKind
                    :hive.schemas.git/conflict-region ConflictRegion
                    :hive.schemas.git/conflict Conflict :hive.schemas.git/stash Stash
                    :hive.schemas.git/operation Operation :hive.schemas.git/status Status})