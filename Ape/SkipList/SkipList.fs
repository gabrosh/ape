module SkipList

open System

open SkipListAux

let private makeCopy<'T> (chunk: ResizeArray<'T>) =
    ResizeArray<'T> chunk

type SkipList<'T> (levelsCount: int, maxSize: int) =
    let maxLevel = levelsCount - 1
    
    let mutable myCount      = 0
    let mutable myNodesCount = 1

    let toUpdate = Array.zeroCreate levelsCount
    let accSizes = Array.zeroCreate levelsCount

    member val Start = {
        nexts    = Array.create levelsCount ValueNone
        sizes    = Array.create levelsCount 0
        chunk    = ResizeArray<'T> ()
        isInUndo = false
    }

    // getting the nodes -------------------------------------------------------

    member inline private _.GetNodeAux
        index (startNode: Node<'T>) startAccSize level =
        
        let rec loop node accSize =
            match node.nexts[level] with
            | ValueSome nextNode ->
                let nextAccSize = accSize + node.sizes[level]    
                if nextAccSize > index then
                    struct (node, accSize)
                else
                    loop nextNode nextAccSize
            | ValueNone ->
                struct (node, accSize)

        loop startNode startAccSize

    member inline private _.GetNodeAuxForRemove
        index (nodeToRemove: Node<'T>) (startNode: Node<'T>) startAccSize level =

        let rec loop node accSize =
            match node.nexts[level] with
            | ValueSome nextNode ->
                if Object.ReferenceEquals (nextNode, nodeToRemove) then
                    struct (node, accSize)
                else
                    let nextAccSize = accSize + node.sizes[level]
                    if nextAccSize > index then
                        // nodeToRemove won't be found on this level.
                        struct (startNode, startAccSize)
                    else
                        loop nextNode nextAccSize
            | ValueNone ->
                // nodeToRemove was not found on this level.
                struct (startNode, startAccSize)

        loop startNode startAccSize

    member private this.GetNodeAndAccSize index =
        let mutable node = this.Start
        let mutable accSize = 0

        for level = maxLevel downto 0 do
            let struct (a, b) = this.GetNodeAux index node accSize level 
            node    <- a
            accSize <- b

        struct (node, accSize)

    member private this.SetNodesToUpdateAndAccSizes index =
        let mutable node = this.Start
        let mutable accSize = 0

        for level = maxLevel downto 0 do
            let struct (a, b) = this.GetNodeAux index node accSize level 
            node    <- a
            accSize <- b

            toUpdate[level] <- node
            accSizes[level] <- accSize

    member private this.SetNodesToUpdateForRemove index (nodeToRemove: Node<'T>) =
        let mutable node = this.Start
        let mutable accSize = 0

        for level = maxLevel downto 0 do
            let struct (a, b) = this.GetNodeAuxForRemove index nodeToRemove node accSize level 
            node    <- a
            accSize <- b

            toUpdate[level] <- node

    member private _.UpdateNodeSizes sizeDelta =
        for level = maxLevel downto 0 do
            let node = toUpdate[level]

            node.sizes[level] <- node.sizes[level] + sizeDelta

    // updating the data structure ---------------------------------------------

    member private _.InsertNewNode (chunk: ResizeArray<'T>) chunkSize =
        let nodeSize = toUpdate[0].sizes[0]
        let nextAccSize = accSizes[0] + nodeSize

        let newNode: Node<'T> = {
            nexts    = Array.create levelsCount ValueNone
            sizes    = Array.create levelsCount 0
            chunk    = chunk
            isInUndo = false
        }

        let newNodeLevel = getNewNodeLevel maxLevel

        for level = maxLevel downto newNodeLevel + 1 do
            let node = toUpdate[level]
            
            node.sizes[level] <- node.sizes[level] + chunkSize

        for level = newNodeLevel downto 0 do
            let node = toUpdate[level]

            // The whole span between node and its successor is a + b.
            let a = nextAccSize - accSizes[level]
            let b = node.sizes[level] - a

            newNode.nexts[level] <- node.nexts[level]
            newNode.sizes[level] <- chunkSize + b

            node.nexts[level] <- ValueSome newNode
            node.sizes[level] <- a

            toUpdate[level] <- newNode
            accSizes[level] <- nextAccSize

        myNodesCount <- myNodesCount + 1

    member private _.RemoveNode (nodeToRemove: Node<'T>) =
        for level = maxLevel downto 0 do
            let node = toUpdate[level]

            match node.nexts[level] with
            | ValueSome nextNode ->
                if Object.ReferenceEquals (nextNode, nodeToRemove) then
                    node.nexts[level] <- nodeToRemove.nexts[level]
                    let sizeDelta = nodeToRemove.sizes[level]
                    node.sizes[level] <- node.sizes[level] + sizeDelta
            | ValueNone ->
                ()

        myNodesCount <- myNodesCount - 1

    member private this.RemoveFirstNodeIfPossible () =
        let firstNode = this.Start

        match firstNode.nexts[0] with
        | ValueSome secondNode ->

            for level = maxLevel downto 0 do
                match firstNode.nexts[level] with
                | ValueSome nextNode ->
                    // The first two nodes are linked on this level?
                    if Object.ReferenceEquals (nextNode, secondNode) then
                        firstNode.nexts[level] <- secondNode.nexts[level]
                        firstNode.sizes[level] <- secondNode.sizes[level]
                | ValueNone ->
                    ()    

            firstNode.chunk    <- secondNode.chunk
            firstNode.isInUndo <- secondNode.isInUndo

            myNodesCount <- myNodesCount - 1

        | ValueNone ->
            ()

    // clearing the data structure ---------------------------------------------

    member private this.ClearAux () =
        let firstNode = this.Start

        let levelsCount = firstNode.nexts.Length

        firstNode.nexts    <- Array.create levelsCount ValueNone
        firstNode.sizes    <- Array.create levelsCount 0
        firstNode.chunk    <- ResizeArray<'T> ()
        firstNode.isInUndo <- false

        myCount      <- 0
        myNodesCount <- 1

    // undo support ------------------------------------------------------------

    member private this.GetUndoNodesAux () =
        let result = ResizeArray<ResizeArray<'T>> myNodesCount

        let rec loop node =
            result.Add node.chunk

            node.isInUndo <- true

            match node.nexts[0] with
            | ValueSome nextNode ->
                loop nextNode
            | ValueNone ->
                ()

        loop this.Start

        result

    member private _.AppendUndoNode (chunk: ResizeArray<'T>) chunkSize =
        let newNode: Node<'T> = {
            nexts    = Array.create levelsCount ValueNone
            sizes    = Array.create levelsCount 0
            chunk    = chunk
            isInUndo = true
        }

        let newNodeLevel = getNewNodeLevel maxLevel

        for level = maxLevel downto newNodeLevel + 1 do
            let node = toUpdate[level]
            
            node.sizes[level] <- node.sizes[level] + chunkSize

        for level = newNodeLevel downto 0 do
            let node = toUpdate[level]

            newNode.nexts[level] <- node.nexts[level]
            newNode.sizes[level] <- chunkSize

            node.nexts[level] <- ValueSome newNode

            toUpdate[level] <- newNode    

        myCount      <- myCount + chunkSize
        myNodesCount <- myNodesCount + 1

    member private this.AppendUndoNodes (chunks: ResizeArray<ResizeArray<'T>>) =
        for level = 0 to maxLevel do
            toUpdate[level] <- this.Start

        for chunk in chunks do
            this.AppendUndoNode chunk chunk.Count

    // public ------------------------------------------------------------------

    /// Count of items in the data structure.
    member _.Count = myCount

    /// Count of nodes in the data structure.
    member _.NodesCount = myNodesCount

    /// Gets or sets the item at the specified index.
    member this.Item
        with get index       = this.Get index
        and  set index value = this.Set index value
    
    /// Gets item at index.
    member this.Get index =
        if index < 0 || index > myCount then
            raise (new ArgumentOutOfRangeException "index")

        let struct (node, accSize) = this.GetNodeAndAccSize index

        node.chunk[index - accSize]

    /// Sets item at index.
    member this.Set index (item: 'T) =
        if index < 0 || index > myCount then
            raise (new ArgumentOutOfRangeException "index")

        let struct (node, accSize) = this.GetNodeAndAccSize index
        
        if node.isInUndo then
            node.chunk <- makeCopy node.chunk
            node.isInUndo <- false

        node.chunk[index - accSize] <- item

    /// Returns count items starting at index as a sequence.
    member this.GetRangeSeq index count =
        if index < 0 || index + count > myCount then
            raise (new ArgumentOutOfRangeException "index, count")

        let struct (startNode, accSize) = this.GetNodeAndAccSize index

        seq {
            let mutable node = ValueSome startNode
            let mutable pos  = index - accSize

            let mutable currentCount = 0

            while currentCount < count do
                let node' = node.Value
                let nodeChunk = node'.chunk

                while currentCount < count && pos < nodeChunk.Count do
                    yield nodeChunk[pos]
                    currentCount <- currentCount + 1
                    pos <- pos + 1

                node <- node'.nexts[0]
                pos  <- 0
        }

    /// Inserts item at index.
    member this.Insert index (item: 'T) =
        if index < 0 || index > myCount then
            raise (new ArgumentOutOfRangeException "index")

        this.SetNodesToUpdateAndAccSizes index

        let node     = toUpdate[0]
        let insertAt = index - accSizes[0]

        // Item can be inserted into node.chunk without exceeding maxSize?
        if node.chunk.Count + 1 <= maxSize then
            if node.isInUndo then
                node.chunk <- makeCopy node.chunk
                node.isInUndo <- false

            node.chunk.Insert (insertAt, item)
            this.UpdateNodeSizes 1
        else
            use chunks =
                // Don't append to the last and full node.
                if insertAt = maxSize then
                    (getChunksWithItem item).GetEnumerator ()
                else
                    if node.isInUndo then
                        node.isInUndo <- false

                    let chunks' =
                        (combineWithItem node.chunk insertAt item (maxSize / 2) maxSize).GetEnumerator ()

                    chunks'.MoveNext () |> ignore
                    let firstChunk = chunks'.Current
                    this.UpdateNodeSizes (firstChunk.Count - node.chunk.Count)
                    node.chunk <- firstChunk

                    chunks'

            while chunks.MoveNext () do
                let chunk = chunks.Current
                this.InsertNewNode chunk chunk.Count

        myCount <- myCount + 1

    /// Inserts items with given itemsCount at index.
    member this.InsertItems index (items: 'T seq) itemsCount =
        if index < 0 || index > myCount then
            raise (new ArgumentOutOfRangeException "index")
        if itemsCount = 0 then
            raise (new ArgumentException "items")

        this.SetNodesToUpdateAndAccSizes index

        let node     = toUpdate[0]
        let insertAt = index - accSizes[0]

        // Items can be inserted into node.chunk without exceeding maxSize?
        if node.chunk.Count + itemsCount <= maxSize then
            if node.isInUndo then
                node.chunk <- makeCopy node.chunk
                node.isInUndo <- false

            node.chunk.InsertRange (insertAt, items)
            this.UpdateNodeSizes itemsCount
        else
            use chunks =
                // Don't append to the last and full node.
                if insertAt = maxSize then
                    (getChunksWithItems items maxSize).GetEnumerator ()
                else
                    if node.isInUndo then
                        node.isInUndo <- false

                    let chunks' =
                        (combineWithItems node.chunk insertAt items (maxSize / 2) maxSize).GetEnumerator ()

                    chunks'.MoveNext () |> ignore
                    let firstChunk = chunks'.Current
                    this.UpdateNodeSizes (firstChunk.Count - node.chunk.Count)
                    node.chunk <- firstChunk

                    chunks'

            while chunks.MoveNext () do
                let chunk = chunks.Current
                this.InsertNewNode chunk chunk.Count

        myCount <- myCount + itemsCount

    /// Adds item to the end.
    member this.Add (item: 'T) =
        let index = myCount

        this.SetNodesToUpdateAndAccSizes index

        let node = toUpdate[0]

        // Can items be inserted into node.chunk without exceeding maxSize?
        if node.chunk.Count + 1 <= maxSize then
            if node.isInUndo then
                node.chunk <- makeCopy node.chunk
                node.isInUndo <- false

            node.chunk.Add item
            this.UpdateNodeSizes 1
        else
            let chunk = getChunkWithItem item

            this.InsertNewNode chunk chunk.Count

        myCount <- myCount + 1

    /// Adds items with given itemsCount to the end.
    member this.AddItems (items: 'T seq) itemsCount =
        if itemsCount = 0 then
            raise (new ArgumentException "items")

        let index = myCount

        this.SetNodesToUpdateAndAccSizes index

        let node  = toUpdate[0]
        let addAt = index - accSizes[0]

        // Items can be appended to node.chunk without exceeding maxSize?
        if node.chunk.Count + itemsCount <= maxSize then
            if node.isInUndo then
                node.chunk <- makeCopy node.chunk
                node.isInUndo <- false

            node.chunk.AddRange items
            this.UpdateNodeSizes itemsCount
        else
            use chunks =
                // Don't append to the last and full node.
                if addAt = maxSize then
                    (getChunksWithItems items maxSize).GetEnumerator ()
                else
                    if node.isInUndo then
                        node.isInUndo <- false

                    let chunks' =
                        (combineWithItems node.chunk addAt items maxSize maxSize).GetEnumerator ()

                    chunks'.MoveNext () |> ignore
                    let firstChunk = chunks'.Current
                    this.UpdateNodeSizes (firstChunk.Count - node.chunk.Count)
                    node.chunk <- firstChunk

                    chunks'

            while chunks.MoveNext () do
                let chunk = chunks.Current
                this.InsertNewNode chunk chunk.Count

        myCount <- myCount + itemsCount

    /// Removes item at index.
    member this.Remove index =
        this.RemoveRange index 1

    /// Removes count items starting at index.
    member this.RemoveRange index count =
        if index < 0 || index + count > myCount then
            raise (new ArgumentOutOfRangeException "index, count")

        let mutable remaining = count

        while remaining > 0 do
            this.SetNodesToUpdateAndAccSizes index

            let node     = toUpdate[0]
            let removeAt = index - accSizes[0]
            let toRemove = min (node.chunk.Count - removeAt) remaining

            if node.isInUndo then
                node.chunk <- makeCopy node.chunk
                node.isInUndo <- false

            node.chunk.RemoveRange (removeAt, toRemove)
            this.UpdateNodeSizes -toRemove
            
            if node.chunk.Count = 0 then
                if Object.ReferenceEquals (node, this.Start) then
                    this.RemoveFirstNodeIfPossible ()
                else
                    this.SetNodesToUpdateForRemove index node

                    this.RemoveNode node

            remaining <- remaining - toRemove

        myCount <- myCount - count

    /// Clears the data structure.
    member this.Clear () =
        this.ClearAux ()

    /// Gets all node chunks in the data structure for undo.
    member this.GetUndoNodes () =
        this.GetUndoNodesAux ()

    /// Sets the data structure to contain all node chunks for undo.
    member this.SetFromUndoNodes (chunks: ResizeArray<ResizeArray<'T>>) =
        this.ClearAux ()

        this.AppendUndoNodes chunks

    // Enumerator for a simple iteration over a collection of items of type obj.
    interface Collections.IEnumerable with
        member this.GetEnumerator () : Collections.IEnumerator =
            (this.GetRangeSeq 0 myCount).GetEnumerator ()
    
    // Enumerator for a simple iteration over a collection of items of type 'T.
    interface Collections.Generic.IEnumerable<'T> with
        member this.GetEnumerator () : Collections.Generic.IEnumerator<'T> =
            (this.GetRangeSeq 0 myCount).GetEnumerator ()
