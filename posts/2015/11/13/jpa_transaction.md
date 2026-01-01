---
title: JPA에서 transaction 처리 방법
date: 2015-11-13T12:54:17+09:00
published: true
tags: jpa, transaction
---


~~~~~java
@ Transactional(rollbackFor = {RuntimeException.class})
public SomeService {
  @ Autowired SomeRepository repository1;
  @ Autowired AnotherRepository repository2;
  @ Autowired ThirdRepository repository3;
  ...
  @ Transactional(rollbackFor = {RuntimeException.class})
  public void SomeMethod(SomeEntity obj, String someNewValue) {
    try {
      repository1.updateMethod();
      repository2.updateMethod();
      obj.setValue(someNewValue);
      repository3.save(obj);
    } catch (Exception ex) {
      throw new RuntimeException();
    }
  }
}
~~~~~

~~~~~java
class MyService2 {
  @Transactional(noRollbackFor = IllegalArgumentException.class)
  public void validateAddress(Address address) {
    // Some validations using dao
    if (!isValid) {
      throws new IllegalArgumentException();
    }
  }
}
~~~~~
