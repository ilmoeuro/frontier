module Frontier.Tag
    (TaggedObject()
    ,expandObjectTag
    ,preserveObjectTag
    ,taggedObjects
    ) where

import Frontier.Feature (ObjectTag)
import Frontier.Generic

data TaggedObject = TaggedObject ObjectTag [Object]

expandObjectTag :: Object -> Maybe TaggedObject
expandObjectTag obj 
    = expandTag' `fmap` objectTag obj
        where expandTag' tag = TaggedObject tag
                (obj : [obj' | obj' <- tagObject tag, obj /~ obj'])

preserveObjectTag :: Object -> (Object->Object) -> TaggedObject -> TaggedObject
preserveObjectTag obj fn (TaggedObject tag objs)
    = TaggedObject tag
            (fn obj : [obj' | obj' <- objs, obj /~ obj'])
                  
taggedObjects :: TaggedObject -> [Object]
taggedObjects (TaggedObject _ objs) = objs