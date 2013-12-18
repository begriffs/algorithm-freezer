function find(t, elt) {
  if(!t) { return null; }

  if(t.item === elt) {
    return t;
  } else if (t.item < elt) {
    return find(t.right, elt);
  } else {
    return find(t.left, elt);
  }
}

function min(t) {
  if(!t.left) { return t.item; }
  return min(t.left);
}

function max(t) {
  if(!t.right) { return t.item; }
  return min(t.right);
}

function in_order(t, f) {
  if(!t) { return; }

  in_order(t.left, f);
  f(t.item);
  in_order(t.right, f);
}

function insert(t, elt) {
  if(!t) { return { item: elt }; }
  if(elt == t.item) { return t; }
  if(elt < t.item) {
    return { item: t.item, left: insert(t.left, elt), right: t.right, parent: t };
  }
  return { item: t.item, left: t.left, right: insert(t.right, elt), parent: t };
}

function leftmost_descendant(t) {
  if(!t || !t.left) { return t; }
  return leftmost_descendant(t.left);
}

function successor(t) {
  if(t && t.right) {
    return leftmost_descendant(t.right);
  }
  return null;
}

function replace_node_in_parent(t, t2) {
  var p = t.parent;
  if(p) {
    if(p.left.item == t.item) {
      p.left = t2;
    } else {
      p.right = t2;
    }
  }
  if(t2) {
    t2.parent = p;
  }
}

function remove(t, elt) {
  var victim = find(t, elt);
  if(victim.left && victim.right) {
    var succ    = successor(victim);
    victim.item = succ.item; // relabel the victim as its successor
    replace_node_in_parent(succ, null); // remove successor node from tree
  } else if(victim.left) {
    replace_node_in_parent(victim, victim.left);
  } else if(victim.right) {
    replace_node_in_parent(victim, victim.right);
  } else {
    replace_node_in_parent(victim, null);
  }
}
