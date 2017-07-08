resource "aws_instance" "test" {
  ami = ""
  instance_type = ""

}
resource "null_resource " "a" {

}

data "terraform_remote_state" "a" {


}