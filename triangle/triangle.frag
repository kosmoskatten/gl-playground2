#version 330 core

uniform vec3 triColor;

out vec4 color;

void main()
{
  color = vec4(triColor, 1.0);
}
