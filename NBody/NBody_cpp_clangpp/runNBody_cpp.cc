#include <fstream>
#include <iostream>
#include <array>
#include <vector>
#include <cmath>
#include <tuple>
#include <chrono>

inline double Get_Forces(std::array<std::array<double, 9>, 2000>& particles, const double& m ,
                              const double& sigma, const double& eps, const double& cuttoff, const double& box_x,
							  const double& box_y, const double box_z){
	const double cuttoffsq = std::pow(sigma*cuttoff, 2);
	const double sigma6 = std::pow(sigma,6);
	const double sigma12 = std::pow(sigma, 12);
	double pot=0.0;
	const int N = particles.size();
	double x1, x2, y1, y2, z1, z2, X, Y, Z, F, rsq, r2, r6;
	for (int i=0; i<N-1; i++){
		x1 = particles[i][0];
		y1 = particles[i][1];
		z1 = particles[i][2];
		for (int j=i+1; j<N; j++){

			x2 = particles[j][0];
			y2 = particles[j][1];
			z2 = particles[j][2];
			
			X = x2 - x1;
            Y = y2 - y1;
            Z = z2 - z1;
            
            X  -= box_x * std::round(X/box_x);
            Y  -= box_y * std::round(Y/box_y);
            Z  -= box_z * std::round(Z/box_z);
			
			rsq = X*X + Y*Y + Z*Z;
			if (rsq <= cuttoffsq){
				r2 = 1.0/rsq;
				r6 = r2*r2*r2;
				
				pot += 4.0*eps*(sigma12*r6 - sigma6)*r6;
				F = 48.0*eps*(-sigma12*r6 + 0.5*sigma6)*r2*r6;

				particles[i][6] += F/m*X;
                particles[i][7] += F/m*Y;
                particles[i][8] += F/m*Z;
                particles[j][6] -= F/m*X;
                particles[j][7] -= F/m*Y;
                particles[j][8] -= F/m*Z;
			}
		}
	}
	return pot;
}

std::vector<double> VelocityVerlet(std::array<std::array<double, 9>, 2000>& particles, 
                      const double cuttoff, const double m, const double eps, 
					  const double sigma, const double box_x, const double box_y, 
					  const double box_z, const double dt){
	std::array<std::array<double, 3>, 2000> Forces_old;
	const int N=particles.size();
	double Ekin=0.0, pot;
	std::vector<double> output;

    for (int i=0; i<N; i++){
        particles[i][0] += particles[i][3]*dt + 0.5*particles[i][6]*dt*dt;
        particles[i][1] += particles[i][4]*dt + 0.5*particles[i][7]*dt*dt;
        particles[i][2] += particles[i][5]*dt + 0.5*particles[i][8]*dt*dt;
        
        if (particles[i][0] <= 0){
            particles[i][0] = particles[i][0] + box_x;
		}
        else if (particles[i][0] >= box_x){
            particles[i][0] = particles[i][0] - box_x;
		}
        if (particles[i][1] <= 0){
            particles[i][1] = particles[i][1] + box_y;
		}
        else if (particles[i][1] >= box_y){
            particles[i][1] = particles[i][1] - box_y;
		}
        if (particles[i][2] <= 0){
            particles[i][2] = particles[i][2] + box_z;
		}
        else if (particles[i][2] >= box_z){
            particles[i][2] = particles[i][2] - box_z;
		}
        
        Forces_old[i][0] = particles[i][6];
        Forces_old[i][1] = particles[i][7];
        Forces_old[i][2] = particles[i][8];
        particles[i][6] = 0.0;
        particles[i][7] = 0.0;
        particles[i][8] = 0.0;
	}
        
    pot = Get_Forces(particles, m, sigma, eps, cuttoff, box_x, box_y, box_z);
    
    for (int i=0; i<N; i++){
        particles[i][3] += 0.5*(Forces_old[i][0]+particles[i][6])*dt;
        particles[i][4] += 0.5*(Forces_old[i][1]+particles[i][7])*dt;
        particles[i][5] += 0.5*(Forces_old[i][2]+particles[i][8])*dt;
        
        Ekin += 0.5*m*(std::pow(particles[i][3],2)+std::pow(particles[i][4],2)+std::pow(particles[i][5],2));
	}
	output.push_back(pot);
	output.push_back(Ekin);
	return output;
}

int main(){
	double cuttoff, dt, m, eps, sigma, N, box_x, box_y, box_z;
	std::vector<double> energies;
	int steps;
	std::ofstream outfile, timefile;
	std::array<std::array<double, 9>, 2000> particles;
	std::fstream fileparticles("particlesdone.txt");
	std::fstream fileparameters("parametersdone.txt");
	std::chrono::time_point<std::chrono::system_clock> start, end;

	for (int i = 0; i < 2000; i++) {
		for (int j = 0; j < 9; j++) {
			fileparticles >> particles[i][j];
		}
	}

	fileparameters >> cuttoff;
	fileparameters >> dt;
	fileparameters >> m;
	fileparameters >> eps;
	fileparameters >> sigma;
	fileparameters >> N; //Is not used
	fileparameters >> box_x;
	fileparameters >> box_y;
	fileparameters >> box_z;
    steps = 1000;
	outfile.open("output.txt");
	timefile.open("timing.txt");
	
	start = std::chrono::system_clock::now();
	for (int step=1; step<steps+1; step++){
		energies = VelocityVerlet(particles, cuttoff, m, eps, sigma, box_x, box_y, box_z, dt);
		outfile << energies[1] << ' ' << energies[0] << '\n';
	}
	end = std::chrono::system_clock::now();
	std::chrono::duration<double> elapsed_seconds = end-start;
	timefile << elapsed_seconds.count() << '\n';
	outfile.close();
	timefile.close();
}
