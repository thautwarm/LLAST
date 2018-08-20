int m(int r, int y, int z){
    if(r > y){
      return z + y;
    }
    else if (y > z){
      return r + z;
    }
    else{
      return r + y;
    }

};
