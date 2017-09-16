#include <fstream>
#include <iostream>
#include <cmath>
#include <chrono>
#include <array>
#include <vector>
using namespace std;


double factorial2(const double& n){
	const int n_range=int(n);
	double out = 1.0;
	if (n > 0.0){
		for (int i=0; i<(n_range+1)/2; i++){
			out *= (n-2*i);
		}
	}
	return out;
}

double boys(const double& m, const double& z){
	const double pi=3.141592653589793238462643383279;
	double Fcheck, outval, temp1, F;
	if (z > 25.0){
		F = factorial2(2*m-1)/pow(2.0, m+1)*sqrt(pi/pow(z,2*m+1));
	}
	else {
		F = 0.0;
		temp1 = factorial2(2*m-1);
		for (int i=0;i<100;i++){
			Fcheck = F;
			F += (temp1*pow(2.0*z,i))/(factorial2(2*m+2*i+1));
			Fcheck -= F;
			if (fabs(Fcheck) < 10e-12){
				break;
			}
		}
		F *= exp(-z);
	}
	return F;
}

int R(const int& l1l2,
	const int& m1m2, const int& n1n2, const double& Cx, const double&  Cy,
	const double& Cz, const double& Px, const double& Py, const double& Pz,
	const double& p, vector<vector<vector<double>>>& R1, 
	vector<vector<vector<vector<double>>>>& Rbuffer){
	int exclude_from_n;
	double RPC, PCx, PCy, PCz, outputvalue, F, val;
	
	PCx = Px-Cx;
	PCy = Py-Cy;
	PCz = Pz-Cz;
	RPC = sqrt(pow(PCx,2)+pow(PCy,2)+pow(PCz,2));
	for (int t=0;t<l1l2+1;t++){
		for (int u=0;u<m1m2+1;u++){
			for (int v=0;v<n1n2+1;v++){
				if ((t == 0) && (u==0)){
					exclude_from_n = v;
				}
				else if (t == 0){
					exclude_from_n = n1n2 + u;
				}
				else {
					exclude_from_n = n1n2 + m1m2 + t;
				}
				for (int n=0;n<l1l2+m1m2+n1n2+1-exclude_from_n;n++){
					val = 0.0;
					if ((t == 0) && (u==0) && (v==0)){
						Rbuffer[t][u][v][n] = pow(-2.0*p,n)*boys(n,p*RPC*RPC);
					}
					else {
						if ((t == 0) && (u==0)){
							if (v>1){
								val += (v-1)*Rbuffer[t][u][v-2][n+1];
							}
							val += PCz*Rbuffer[t][u][v-1][n+1];
						}
						else if (t==0){
							if (u>1){
								val += (u-1)*Rbuffer[t][u-2][v][n+1];
							}
							val += PCy*Rbuffer[t][u-1][v][n+1];
						}
						else{
							if (t>1){
								val += (t-1)*Rbuffer[t-2][u][v][n+1];
							}
							val += PCx*Rbuffer[t-1][u][v][n+1];
						}
						Rbuffer[t][u][v][n] = val;
					}
					if (n==0){
						R1[t][u][v] = Rbuffer[t][u][v][n];
					}
				}
			}
		}
	}
}

int main(){
	double Cx, Cy, Cz, Px, Py, Pz, p;
	int l1l2, m1m2, n1n2;
	vector<vector<vector<double>>> R1buffer;
	vector<vector<vector<vector<double>>>> Rbuffer;
	ofstream outfile, timefile;
	array<array<double, 10>, 33777> parameters;
	array<array<double, 7>, 33777> integralfloat;
	array<array<int, 3>, 33777> integralint;
	fstream parametersfile("integralinput.txt");
	chrono::time_point<chrono::system_clock> start, end;
	
	for (int i = 0; i < 33777; i++) {
		for (int j = 0; j < 10; j++) {
			parametersfile >> parameters[i][j];
		}
	}
	
	for (int i=0; i<33777; i++){
		integralint[i][0] = (int)parameters[i][0];
		integralint[i][1] = (int)parameters[i][1];
		integralint[i][2] = (int)parameters[i][2];
		integralfloat[i][0] = parameters[i][3];
		integralfloat[i][1] = parameters[i][4];
		integralfloat[i][2] = parameters[i][5];
		integralfloat[i][3] = parameters[i][6];
		integralfloat[i][4] = parameters[i][7];
		integralfloat[i][5] = parameters[i][8];
		integralfloat[i][6] = parameters[i][9];
	}
	
	for (int i=0; i<5; i++){
		R1buffer.push_back(vector<vector<double>>());
		for (int j=0; j<5; j++){
			R1buffer[i].push_back(vector<double>());
			for (int k=0; k<5; k++){
				R1buffer[i][j].push_back(0.0);
			}
		}
	}
	
	for (int i=0; i<5; i++){
		Rbuffer.push_back(vector<vector<vector<double>>>());
		for (int j=0; j<5; j++){
			Rbuffer[i].push_back(vector<vector<double>>());
			for (int k=0; k<5; k++){
				Rbuffer[i][j].push_back(vector<double>());
				for (int l=0; l<13; l++){
					Rbuffer[i][j][k].push_back(0.0);
				}
			}
		}
	}

	timefile.open("timing.txt");
	start = chrono::system_clock::now();
	for (int j=0; j<100; j++){
		outfile.open("output.txt");
		for (int i=0; i<33777; i++){
			l1l2 = integralint[i][0];
			m1m2 = integralint[i][1];
			n1n2 = integralint[i][2];
			Cx = integralfloat[i][0];
			Cy = integralfloat[i][1];
			Cz = integralfloat[i][2];
			Px = integralfloat[i][3];
			Py = integralfloat[i][4];
			Pz = integralfloat[i][5];
			p = integralfloat[i][6];
			R(l1l2, m1m2, n1n2, Cx, Cy, Cz, Px, Py, Pz, p, R1buffer, Rbuffer);
			outfile << R1buffer[l1l2][m1m2][n1n2] << '\n';
		}
		outfile.close();
	}
	end = chrono::system_clock::now();
	chrono::duration<double> elapsed_seconds = end-start;
	timefile << elapsed_seconds.count() << '\n';
	timefile.close();
}