module OAD_intrinsics
end module
module theModule
  use OAD_active
  implicit none
  integer,parameter,private :: a=1
end module theModule

program privateModuleDecl
  use OAD_active
  use OAD_intrinsics
  use theModule
  implicit none
  integer :: a
  
  a = 7
  print *,'a = ',a
end program privateModuleDecl

