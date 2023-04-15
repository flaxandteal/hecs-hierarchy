use std::mem;
const STACK_SIZE: usize = 64;
use smallvec::{smallvec, SmallVec};
use hecs::{QueryBorrow};
use hecs::{RefMut, Ref};
use std::marker::PhantomData;
use std::collections::VecDeque;

use hecs::{DynamicBundle, Entity, World, Component};
use hecs_schedule::{error::Result, GenericWorld};
use crate::{
    Child,
    Parent,
};

pub struct BreadthFirstIterator<'a, W, T> {
    world: &'a W,
    marker: PhantomData<T>,
    queue: VecDeque<Entity>,
}

impl<'a, W: GenericWorld + HierarchyDag, T: 'static + Send + Sync> BreadthFirstIterator<'a, W, T> {
    pub(crate) fn new(world: &'a W, root: Entity) -> Self {
        // Add immediate children of root to queue
        let queue = HierarchyDag::children::<T>(world, root).collect();

        Self {
            world,
            queue,
            marker: PhantomData,
        }
    }
}

impl<'a, W: GenericWorld + HierarchyDag, T: 'static + Send + Sync> Iterator
    for BreadthFirstIterator<'a, W, T>
{
    type Item = Entity;

    fn next(&mut self) -> Option<Self::Item> {
        let front = self.queue.pop_front()?;

        // Add any potention children of front to the back of queue
        self.queue.extend(HierarchyDag::children::<T>(self.world, front));

        Some(front)
    }
}

pub struct AncestorDepthFirstIter<'a, T: Component> {
    query: QueryBorrow<'a, &'a Child<T>>,
    coquery: QueryBorrow<'a, &'a Knot<T>>,
    current: Entity,
    marker: PhantomData<T>,
    stack: SmallVec<[AncestorStackFrame; STACK_SIZE]>,
    // Potentially levels^2
    labelled: SmallVec<[u32; STACK_SIZE * STACK_SIZE]>,
}

impl<'a, T: Component> AncestorDepthFirstIter<'a, T> {
    pub(crate) fn new<W: GenericWorld>(world: &'a W, current: Entity) -> Self {
        Self {
            query: world.try_query().unwrap(),
            coquery: world.try_query().unwrap(),
            current,
            stack: smallvec![],
            marker: PhantomData,
            labelled: smallvec![],
        }
    }
}

impl<'a, T: Component> Iterator for AncestorDepthFirstIter<'a, T> {
    type Item = Entity;

    fn next(&mut self) -> Option<Self::Item> {
        let mut current = {
            match self.query.view().get(self.current) {
                Some(child) => {
            // println!["{} PARENT", self.current.id()];
                    let parent = child.parent;
                    if self.labelled.len() > 0 {
                        if !self.labelled.contains(&parent.id()) {
                            self.labelled.push(parent.id());
                            Some(parent)
                        } else {
                            None
                        }
                    } else {
                        Some(parent)
                    }
                },
                None => {
                    if let Some(knot) = self.coquery.view().get(self.current) {
                        self.labelled.push(self.current.id());
            // println!["{} PUSH", self.current.id()];
                        self.stack.push(AncestorStackFrame {
                            current: self.current
                        });
                        Some(knot.child_left.parent)
                    } else {
                        None
                    }
                }
            }
        };
        if current == None {
            current = match self.stack.last_mut() {
                Some(top) => {
                    let view = self.coquery.view();
                    let current = view.get(top.current)?;
// println!["{} POP {} {}", top.current.id(), current.child_left.parent.id(), current.child_right.parent.id()];
                    self.stack.pop();
                    Some(current.child_right.parent)
                },
                None => None
            }
        };
        match current {
            Some(current) => {
                self.current = current;
                Some(current)
            },
            None => None
        }
    }
}

pub trait Coparent<'a, T: Component> {
    fn view_first_child(&self, view: &ChildlikeView<T>, parent_id: u32) -> Result<Entity>;
    fn first_child<W: GenericWorld + HierarchyDag>(&self, world: &W, entity: u32) -> Result<Entity>;
}

impl<'a, T: 'static + Send + Sync> Coparent<'a, T> for Parent<T> {
    fn view_first_child(&self, view: &ChildlikeView<T>, parent_id: u32) -> Result<Entity> {
        Ok(view
            .get(self.last_child, parent_id)
            .ok_or_else(|| hecs_schedule::Error::NoSuchEntity(self.last_child))?
            .next)
    }
    /// Query the parent's first child.
    fn first_child<W: GenericWorld + HierarchyDag>(&self, world: &W, entity: u32) -> Result<Entity> {
        Ok(
            match world.try_get::<Child<T>>(self.last_child) {
                Ok(child) => child.next,
                Err(_) => {
                    // FIXME: poor hack to get around no reference to entity in parent
                    let child = world.try_get_child::<T>(self.last_child, entity)?.next;
                    // println!["{}", child.id()];
                    child
                }
            }
        )
    }

}

pub trait HierarchyDag: crate::Hierarchy
where
    Self: Sized,
{
    fn new() -> Self;
    fn children<T: Component>(&self, parent: Entity) -> CochildrenIter<T>;
    fn default() -> Self;
    fn ancestors<T: Component>(&self, child: Entity) -> AncestorDepthFirstIter<T>;
    fn descendants_depth_first<T: Component>(&self, root: Entity) -> DepthFirstIterator<T>;
    fn descendants_duplicating_depth_first<T: Component>(&self, root: Entity) -> DuplicatingDepthFirstIterator<T>;
    fn descendants_breadth_first<T: Component>(
        &self,
        root: Entity,
    ) -> BreadthFirstIterator<Self, T>;
    fn try_get_child<T: Component>(&self, entity: Entity, parent: u32) -> Result<Ref<Child<T>>>;
}

//type AltChild<T> = Child<T>;
pub struct Knot<T> {
    pub child_left: Child<T>,
    pub child_right: Child<T>,
    marker: PhantomData<T>,
}

impl<T> Knot<T> {
    pub(crate) fn new(child_left: Child<T>, child_right: Child<T>) -> Self {
        Self {
            child_left: child_left,
            child_right: child_right,
            marker: PhantomData,
        }
    }
}

#[derive(Clone)]
#[derive(Copy)]
#[derive(PartialEq)]
pub enum DetachMode {
    IGNORE = 1,
    PRUNE = 2,
    KEEP = 3,
    COPY = 4
}

pub trait HierarchyDagMut: crate::HierarchyMut {
    /// Attach `child` to `parent`. Parent does not require an existing `Parent component`. Returns
    /// the passed child.
    /// *Note*: The entity needs to be explicitly detached before being removed.
    fn _detach<T: Component>(&self, data: &mut Child<T>, child: Entity) -> Result<Option<Entity>>;
    fn _detach_tidy<T: Component>(&mut self, child: Entity, mode: DetachMode) -> Result<()>;
    fn knot<T: Component, C: DynamicBundle>(&mut self, parent1: Entity, parent2: Entity, components: C) -> Result<Entity>;
    fn try_get_child_mut<T: Component>(&self, entity: Entity, parent: u32) -> Result<RefMut<Child<T>>>;
    fn attach_new<T: Component, C: DynamicBundle>(
        &mut self,
        parent: Entity,
        components: C,
    ) -> Result<Entity>;
    fn detach_all<T: Component>(&mut self, entity: Entity) -> Result<()>;
    fn detach_children<T: Component>(&mut self, parent: Entity) -> Result<Vec<Entity>>;
    fn despawn_children<T: Component>(&mut self, parent: Entity) -> Result<()>;
    fn detach<T: Component>(&mut self, child: Entity, mode: DetachMode) -> Result<()>;
    fn despawn_all<T: Component>(&mut self, parent: Entity);
    fn attach<T: Component>(&mut self, child: Entity, parent: Entity) -> Result<Entity>;
}

#[derive(Debug)]
struct AncestorStackFrame {
    current: Entity,
}

#[derive(Debug)]
struct StackFrame {
    current: Entity,
    parent_id: u32,
    remaining: usize,
}

pub struct DuplicatingDepthFirstIterator<'a, T: Component> {
    children: Childlike<'a, T>,
    parents: QueryBorrow<'a, &'a Parent<T>>,
    marker: PhantomData<T>,
    /// Since StackFrame is so small, use smallvec optimizations
    stack: SmallVec<[StackFrame; STACK_SIZE]>,
}

pub struct DepthFirstIterator<'a, T: Component> {
    children: Childlike<'a, T>,
    parents: QueryBorrow<'a, &'a Parent<T>>,
    marker: PhantomData<T>,
    /// Since StackFrame is so small, use smallvec optimizations
    stack: SmallVec<[StackFrame; STACK_SIZE]>,
}

pub struct ChildlikeView<'q, 'a, T: Component> {
    children: hecs::View<'q, &'a Child<T>>,
    knots: hecs::View<'q, &'a Knot<T>>,
}

pub struct Childlike<'a, T: Component> {
    children: QueryBorrow<'a, &'a Child<T>>,
    knots: QueryBorrow<'a, &'a Knot<T>>,
}

impl<'a, T: Component> Childlike<'a, T> {
    pub(crate) fn view(&mut self) -> ChildlikeView<'_, 'a, T> {
        ChildlikeView::new(self.children.view(), self.knots.view())
    }
}

impl<'q, 'a, T: Component> ChildlikeView<'q, 'a, T> {
    pub(crate) fn new(children: hecs::View<'q, &'a Child<T>>, knots: hecs::View<'q,  &'a Knot<T>>) -> Self {
        Self {
            children,
            knots
        }
    }

    pub(crate) fn get<'m>(&'m self, entity: Entity, parent_id: u32) -> Option<&Child<T>> {
        // println!["Entity {}", entity.id()];
        match self.children.get(entity) {
            Some(child) => Some(&child),
            None => {
                match self.knots.get(entity) {
                    Some(knot) if knot.child_left.parent.id() == parent_id => Some(&knot.child_left),
                    Some(knot) => Some(&knot.child_right),
                    None => None
                }
            }
        }
    }
}

impl<'a, T: Component> DuplicatingDepthFirstIterator<'a, T> {
    pub(crate) fn new<W: GenericWorld + HierarchyDag>(world: &'a W, root: Entity) -> Self {
        let children = Childlike {
            children: world.try_query().unwrap(),
            knots: world.try_query().unwrap()
        };
        let mut parents = world.try_query::<&Parent<T>>().unwrap();

        let stack = parents
            .view()
            .get(root)
            .and_then(|parent| {
                let first_child = Coparent::first_child(&(*parent), world, root.id()).ok()?;
                Some(smallvec![StackFrame {
                    current: first_child,
                    parent_id: root.id(),
                    remaining: parent.num_children,
                }])
            })
            .unwrap_or_default();

        Self {
            children,
            parents,
            stack,
            marker: PhantomData,
        }
    }
}

impl<'a, T: Component> Iterator for DuplicatingDepthFirstIterator<'a, T> {
    type Item = (Entity, Entity);

    fn next(&mut self) -> Option<Self::Item> {
        // The the topmost stackframe
        let top = self.stack.last_mut()?;

        // There are more children in current stackframe
        if top.remaining > 0 {
            let current = top.current;

            let children = &mut self.children;
            let view = children.view();
            let data = {
                let got = view.get(top.current, top.parent_id);
                got.unwrap()
            };

            // Go to the next child in the linked list of children
            top.current = data.next;
            top.remaining -= 1;

            // If current is a parent, push a new stack frame with the first child
            if let Some(parent) = self.parents.view().get(current) {
                self.stack.push(StackFrame {
                    current: Coparent::view_first_child(parent, &view, current.id()).unwrap(),
                    parent_id: current.id(),
                    remaining: parent.num_children,
                })
            }

            // println!["data {} {}", current.id(), data.parent.id()];
            Some((current, data.parent))
        } else {
            // End of linked list of children, pop stack frame
            self.stack.pop();
            self.next()
        }
    }
}

impl<'a, T: Component> DepthFirstIterator<'a, T> {
    pub(crate) fn new<W: GenericWorld + HierarchyDag>(world: &'a W, root: Entity) -> Self {
        let children = Childlike {
            children: world.try_query().unwrap(),
            knots: world.try_query().unwrap()
        };
        let mut parents = world.try_query::<&Parent<T>>().unwrap();

        let stack = parents
            .view()
            .get(root)
            .and_then(|parent| {
                let first_child = Coparent::first_child(&(*parent), world, root.id()).ok()?;
                Some(smallvec![StackFrame {
                    current: first_child,
                    parent_id: root.id(),
                    remaining: parent.num_children,
                }])
            })
            .unwrap_or_default();

        Self {
            children,
            parents,
            stack,
            marker: PhantomData,
        }
    }
}

impl<'a, T: Component> Iterator for DepthFirstIterator<'a, T> {
    type Item = Entity;

    fn next(&mut self) -> Option<Self::Item> {
        // The the topmost stackframe
        let top = self.stack.last_mut()?;

        // There are more children in current stackframe
        if top.remaining > 0 {
            let current = top.current;

            let children = &mut self.children;
            let view = children.view();
            let data = {
                let got = view.get(top.current, top.parent_id);
                got.unwrap()
            };

            // Go to the next child in the linked list of children
            top.current = data.next;
            top.remaining -= 1;

            // If current is a parent, push a new stack frame with the first child
            if let Some(parent) = self.parents.view().get(current) {
                // println!["PT{} {}", data.parent.id(), current.id()];
                self.stack.push(StackFrame {
                    current: Coparent::view_first_child(parent, &view, current.id()).unwrap(),
                    parent_id: current.id(),
                    remaining: parent.num_children,
                })
            }

            Some(current)
        } else {
            // End of linked list of children, pop stack frame
            self.stack.pop();
            self.next()
        }
    }
}

impl HierarchyDagMut for World {
    fn detach<T: Component>(&mut self, child: Entity, mode: DetachMode) -> Result<()> {
        let mut parents: [Option<Entity>; 2] = [None, None];
        let is_knot = match self.try_get_mut::<Child<T>>(child) {
            Ok(mut data) => {
                let data: &mut Child<T> = &mut data;
                parents[0] = self._detach(data, child)?;
                false
            },
            _ => {
                let mut knot = self.try_get_mut::<Knot<T>>(child)?;
                parents[0] = self._detach(&mut knot.child_left, child)?;
                parents[1] = self._detach(&mut knot.child_right, child)?;
                true
            }
        };
        if is_knot {
            self.try_remove_one::<Knot<T>>(child)?;
        } else {
            self.try_remove_one::<Child<T>>(child)?;
        };
        parents.iter().for_each(|parent| {
            match parent {
                Some(parent) => { self.try_remove_one::<Parent<T>>(*parent).unwrap(); },
                _ => ()
            }
        });
        self._detach_tidy::<T>(child, mode)?;
        Ok(())
    }

    fn _detach<T: Component>(&self, data: &mut Child<T>, child: Entity) -> Result<Option<Entity>> {
        let parent = data.parent;
        let prev = data.prev;
        let next = data.next;

        if prev != child {
            self.try_get_child_mut::<T>(prev, parent.id())?.next = next;
        }
        if next != child {
            self.try_get_child_mut::<T>(next, parent.id())?.prev = prev;
        }

        let mut parent_component = self.try_get_mut::<Parent<T>>(parent)?;

        parent_component.num_children -= 1;
        let result: Option<Entity> = if parent_component.num_children == 0 {
            Some(parent)
        } else {
            if parent_component.last_child == child {
                parent_component.last_child = prev;
            };
            None
        };

        Ok(result)
    }

    fn _detach_tidy<T: Component>(&mut self, child: Entity, mode: DetachMode) -> Result<()> {
        if mode == DetachMode::IGNORE {
            return Ok(())
        }

        let mut split_knots: std::collections::HashMap<u32, (Entity, u32)> = std::collections::HashMap::new();
        {
            let mut coquery: QueryBorrow<&Knot<T>> = self.try_query()?;
            let view = coquery.view();
            self
                .descendants_duplicating_depth_first::<T>(child)
                .for_each(|(entity, parent)| {
                    // println!["E{} {}", entity.id(), parent.id()];
                    if let Some(split_knot) = view.get(entity) {
                        // println!["S{}", split_knot.child_left.parent.id()];
                        let other_parent = if parent.id() == split_knot.child_left.parent.id() {
                            split_knot.child_right.parent
                        } else {
                            split_knot.child_left.parent
                        };
                        let key = other_parent.id();
                        if split_knots.contains_key(&key) {
                            split_knots.remove(&key);
                        } else {
                            split_knots.insert(key, (entity, parent.id()));
                        }
                    }
                });
            mem::drop(view);
        }
        // println!["{}", split_knots.len()];
        split_knots.iter().for_each(|(_, pair)| {
            // PRUNE mode
            // note that other modes may change the split_knots status of descendent knots
            match mode {
                DetachMode::PRUNE => {
                    let (entity, in_parent): (Entity, u32) = *pair;
                    let left_in = self.try_get::<Knot<T>>(entity).unwrap().child_left.parent.id() == in_parent;
                    {
                        let mut split_knot = self.try_get_mut::<Knot<T>>(entity).unwrap();
                        self._detach(if left_in {
                            &mut split_knot.child_right
                        } else {
                            &mut split_knot.child_left
                        }, entity).unwrap();
                    }

                    let split_knot = self.try_remove_one::<Knot<T>>(entity).unwrap();

                    let in_child = if left_in {
                        split_knot.child_left
                    } else {
                        split_knot.child_right
                    };
                    self.try_insert(entity, (in_child,)).unwrap();
                    // println!["-> {} out: {} in: {}", entity.id(), out_parent, in_parent];
                },
                _ => { panic!["Detach mode not yet implemented"]; }
            }
        });
        Ok(())
    }

    fn attach<T: Component>(&mut self, child: Entity, parent: Entity) -> Result<Entity> {
        let mut maybe_p = self.try_get_mut::<Parent<T>>(parent);
        if let Ok(ref mut p) = maybe_p {
            p.num_children += 1;
            // println!["{} has {}", parent.id(), p.num_children];
            let prev = p.last_child;
            p.last_child = child;

            let mut prev_data = self.try_get_child_mut::<T>(prev, parent.id())?;
            let next = prev_data.next;
            prev_data.next = child;

            mem::drop(prev_data);
            mem::drop(maybe_p);

            // Update backward linking
            {
                let mut next_data = self.try_get_child_mut::<T>(next, parent.id())?;
                next_data.prev = child;
            }

            self.try_insert(child, (Child::<T>::new(parent, next, prev),))?;

            return Ok(child);
        }

        mem::drop(maybe_p);

        // Parent component didn't exist
        self.try_insert(parent, (Parent::<T>::new(1, child),))?;

        self.try_insert(child, (Child::<T>::new(parent, child, child),))?;

        Ok(child)
    }

    fn knot<T: Component, C: DynamicBundle>(&mut self, parent1: Entity, parent2: Entity, components: C) -> Result<Entity> {
        let knot = self.spawn(components);

        let mut children: Vec<Child<T>> = vec![parent1, parent2].iter().map(|parent| -> Result<Child<T>> {
            let mut maybe_p = self.try_get_mut::<Parent<T>>(*parent);
            // println!["Q{}", parent.id()];
            if let Ok(ref mut p) = maybe_p {
                p.num_children += 1;
            // println!["{} has {}", parent.id(), p.num_children];
                let prev = p.last_child;
                p.last_child = knot;

                let mut prev_data = self.try_get_child_mut::<T>(prev, parent.id())?;
                let next = prev_data.next;
                prev_data.next = knot;

                mem::drop(prev_data);
                mem::drop(maybe_p);

                // Update backward linking
                {
                    let mut next_data = self.try_get_child_mut::<T>(next, parent.id())?;
                    next_data.prev = knot;
                }

                return Ok(Child::<T>::new(*parent, next, prev))
            }
            mem::drop(maybe_p);

            // Parent component didn't exist
            self.try_insert(*parent, (Parent::<T>::new(1, knot),))?;
            Ok(Child::<T>::new(*parent, knot, knot))
        }).flatten().collect();

        if let (Some(child2), Some(child1)) = (children.pop(), children.pop()) {
            // println!["P{} {}", child1.parent.id(), child2.parent.id()];
            self.try_insert(knot, (Knot::<T>::new(
                        child1,
                        child2
            ),))?;
        }
        Ok(knot)
    }

    fn try_get_child_mut<T: Component>(&self, entity: Entity, parent: u32) -> Result<RefMut<Child<T>>> {
        match self.get::<'_, &mut Child<T>>(entity) {
            Ok(child) => Ok(child),
            Err(hecs::ComponentError::NoSuchEntity) => Err(hecs_schedule::Error::NoSuchEntity(entity)),
            Err(hecs::ComponentError::MissingComponent(name)) => match self.get::<'_, &mut Knot<T>>(entity) {
                Ok(pair) => {
                    if pair.child_right.parent.id() == parent {
                        Ok(hecs::RefMut::map(pair, |pair| &mut pair.child_right))
                    } else {
                        Ok(hecs::RefMut::map(pair, |pair| &mut pair.child_left))
                    }
                },
                _ => Err(hecs_schedule::Error::MissingComponent(entity, name))
            }
        }
    }

    fn attach_new<T: Component, C: DynamicBundle>(
        &mut self,
        parent: Entity,
        components: C,
    ) -> Result<Entity> {
        let child = self.spawn(components);
        self.attach::<T>(child, parent)
    }

    fn detach_all<T: Component>(&mut self, entity: Entity) -> Result<()> {
        self.detach_children::<T>(entity)?;
        self.detach::<T>(entity, DetachMode::IGNORE)?;
        Ok(())
    }

    /// Detaches all children of parent.
    fn detach_children<T: Component>(&mut self, parent: Entity) -> Result<Vec<Entity>> {
        let children = self.children::<T>(parent).collect::<Vec<Entity>>();

        children.iter().try_for_each(|child| -> Result<_> {
            if self.try_get::<Child<T>>(*child).is_ok() {
                self.try_remove_one::<Child<T>>(*child)?;
            } else {
                self.detach::<T>(*child, DetachMode::IGNORE)?;
            };
            Ok(())
        })?;

        self.remove_one::<Parent<T>>(parent).unwrap();

        Ok(children)
    }

    fn despawn_all<T: Component>(&mut self, parent: Entity) {
        let to_despawn = self
            .descendants_depth_first::<T>(parent)
            .collect::<Vec<_>>();

        // Detach from parent if necessary
        let _ = self.detach::<T>(parent, DetachMode::PRUNE);

        // Should not panic since we just
        to_despawn.iter().for_each(|entity| {
            let _ = self.despawn(*entity);
        });

        let _ = self.despawn(parent);
    }

    fn despawn_children<T: Component>(&mut self, parent: Entity) -> Result<()> {
        let children = self.children::<T>(parent).collect::<Vec<Entity>>();

        children
            .iter()
            .for_each(|child| self.despawn_all::<Child<T>>(*child));

        self.remove_one::<Parent<T>>(parent).unwrap();

        Ok(())
    }
}

pub struct CochildrenIter<'a, T: Component> {
    coquery: QueryBorrow<'a, &'a Knot<T>>,
    query: QueryBorrow<'a, &'a Child<T>>,
    remaining: usize,
    current: Option<Entity>,
    parent: u32,
    marker: PhantomData<T>,
}

impl<'a, T: Component> CochildrenIter<'a, T> {
    pub(crate) fn new<W: GenericWorld>(
        world: &'a W,
        num_children: usize,
        parent: u32,
        current: Option<Entity>,
    ) -> Self {
        Self {
            query: world.try_query().unwrap(),
            coquery: world.try_query().unwrap(),
            remaining: num_children,
            current,
            parent: parent,
            marker: PhantomData,
        }
    }
}

impl<'a, T> Iterator for CochildrenIter<'a, T>
where
    T: Component,
{
    type Item = Entity;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            return None;
        }

        self.remaining -= 1;

        let current = self.current?;
        let view = self.query.view();
        let coview = self.coquery.view();
        let data = match view.get(current) {
            Some(data) => data,
            None => {
                // println!["coview"];
                let knot = coview.get(current)?;
                if knot.child_right.parent.id() == self.parent {
                    &knot.child_right
                } else {
                    &knot.child_left
                }
            }
        };

        self.current = Some(data.next);
        Some(current)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }

    fn count(self) -> usize
    where
        Self: Sized,
    {
        self.remaining
    }
}

impl HierarchyDag for World {
    fn ancestors<T: Component>(&self, child: Entity) -> AncestorDepthFirstIter<T> {
        AncestorDepthFirstIter::new(self, child)
    }

    fn children<T: Component>(&self, parent: Entity) -> CochildrenIter<T> {
        self.try_get::<Parent<T>>(parent)
            .and_then(|p| {
                let first_child = Coparent::first_child(&(*p), self, parent.id())?;
                // println!["{} {}", p.num_children(), parent.id()];

                Ok(CochildrenIter::new(
                    self,
                    p.num_children(),
                    parent.id(),
                    Some(first_child),
                ))
            })
            .unwrap_or_else(move |_| {
                // println!["{}?", parent.id()];
                // Return an iterator that does nothing.
                CochildrenIter::new(self, 0, parent.id(), None)
            })
    }

    fn new() -> Self {
        Self::new()
    }

    fn default() -> Self {
        Self::new()
    }

    fn descendants_breadth_first<T: Component>(
        &self,
        root: Entity,
    ) -> BreadthFirstIterator<Self, T> {
        BreadthFirstIterator::new(self, root)
    }

    fn descendants_depth_first<T: Component>(&self, root: Entity) -> DepthFirstIterator<T> {
        DepthFirstIterator::new(self, root)
    }

    fn descendants_duplicating_depth_first<T: Component>(&self, root: Entity) -> DuplicatingDepthFirstIterator<T> {
        DuplicatingDepthFirstIterator::new(self, root)
    }

    fn try_get_child<T: Component>(&self, entity: Entity, parent: u32) -> Result<Ref<Child<T>>> {
        match self.get::<'_, &Child<T>>(entity) {
            Ok(child) => Ok(child),
            Err(hecs::ComponentError::NoSuchEntity) => Err(hecs_schedule::Error::NoSuchEntity(entity)),
            Err(hecs::ComponentError::MissingComponent(name)) => match self.get::<'_, &Knot<T>>(entity) {
                Ok(pair) => {
                    if pair.child_right.parent.id() == parent {
                        Ok(hecs::Ref::map(pair, |pair| &pair.child_right))
                    } else {
                        Ok(hecs::Ref::map(pair, |pair| &pair.child_left))
                    }
                },
                _ => Err(hecs_schedule::Error::MissingComponent(entity, name))
            }
        }
    }

}

trait WorldExt {
    fn try_insert(&mut self, e: Entity, c: impl DynamicBundle) -> Result<()>;
    fn try_remove_one<C: Component>(&mut self, e: Entity) -> Result<C>;
}

impl WorldExt for World {
    fn try_insert(&mut self, e: Entity, c: impl DynamicBundle) -> Result<()> {
        self.insert(e, c)
            .map_err(|_| hecs_schedule::Error::NoSuchEntity(e))
    }

    fn try_remove_one<C: Component>(&mut self, e: Entity) -> Result<C> {
        self.remove_one::<C>(e)
            .map_err(|_| hecs_schedule::Error::NoSuchEntity(e))
    }
}
