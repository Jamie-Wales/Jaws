import animatePlugin from "tailwindcss-animate"

/** @type {import('tailwindcss').Config} */
export default {
    darkMode: ["class"],
    content: [
        './index.html',
        './src/**/*.{js,ts,jsx,tsx}',
    ],
    theme: {
    	container: {
    		center: true,
    		padding: {
    			DEFAULT: '1rem',
    			sm: '2rem',
    			lg: '4rem',
    			xl: '5rem',
    			'2xl': '6rem'
    		},
    		screens: {
    			sm: '640px',
    			md: '768px',
    			lg: '1024px',
    			xl: '1280px',
    			'2xl': '1536px'
    		}
    	},
    	extend: {
    		fontFamily: {
    			sans: [
    				'Poppins',
    				'system-ui',
    				'sans-serif'
    			]
    		},
    		colors: {
    			border: 'hsl(var(--border))',
    			input: 'hsl(var(--input))',
    			ring: 'hsl(var(--ring))',
    			background: 'hsl(var(--background))',
    			foreground: 'hsl(var(--foreground))',
    			primary: {
    				DEFAULT: 'hsl(var(--primary))',
    				foreground: 'hsl(var(--primary-foreground))'
    			},
    			secondary: {
    				DEFAULT: 'hsl(var(--secondary))',
    				foreground: 'hsl(var(--secondary-foreground))'
    			},
    			destructive: {
    				DEFAULT: 'hsl(var(--destructive))',
    				foreground: 'hsl(var(--destructive-foreground))'
    			},
    			muted: {
    				DEFAULT: 'hsl(var(--muted))',
    				foreground: 'hsl(var(--muted-foreground))'
    			},
    			accent: {
    				DEFAULT: 'hsl(var(--accent))',
    				foreground: 'hsl(var(--accent-foreground))'
    			},
    			popover: {
    				DEFAULT: 'hsl(var(--popover))',
    				foreground: 'hsl(var(--popover-foreground))'
    			},
    			card: {
    				DEFAULT: 'hsl(var(--card))',
    				foreground: 'hsl(var(--card-foreground))'
    			},
    			sidebar: {
    				DEFAULT: 'hsl(var(--sidebar-background))',
    				foreground: 'hsl(var(--sidebar-foreground))',
    				primary: 'hsl(var(--sidebar-primary))',
    				'primary-foreground': 'hsl(var(--sidebar-primary-foreground))',
    				accent: 'hsl(var(--sidebar-accent))',
    				'accent-foreground': 'hsl(var(--sidebar-accent-foreground))',
    				border: 'hsl(var(--sidebar-border))',
    				ring: 'hsl(var(--sidebar-ring))'
    			}
    		},
    		borderRadius: {
    			lg: 'var(--radius)',
    			md: 'calc(var(--radius) - 2px)',
    			sm: 'calc(var(--radius) - 4px)'
    		},
    		keyframes: {
    			'accordion-down': {
    				from: {
    					height: '0'
    				},
    				to: {
    					height: 'var(--radix-accordion-content-height)'
    				}
    			},
    			'accordion-up': {
    				from: {
    					height: 'var(--radix-accordion-content-height)'
    				},
    				to: {
    					height: '0'
    				}
    			},
    			fadeSlideIn: {
    				from: {
    					opacity: 0,
    					transform: 'translateY(10px)'
    				},
    				to: {
    					opacity: 1,
    					transform: 'translateY(0)'
    				}
    			},
    			slideFromLeft: {
    				from: {
    					opacity: 0,
    					transform: 'translateX(-10px)'
    				},
    				to: {
    					opacity: 1,
    					transform: 'translateX(0)'
    				}
    			},
    			floatIn: {
    				'0%': {
    					opacity: 0,
    					transform: 'translateY(30px)'
    				},
    				'100%': {
    					opacity: 1,
    					transform: 'translateY(0)'
    				}
    			},
    			slideInRight: {
    				'0%': {
    					opacity: 0,
    					transform: 'translateX(-30px)'
    				},
    				'100%': {
    					opacity: 1,
    					transform: 'translateX(0)'
    				}
    			},
    			fadeScale: {
    				'0%': {
    					opacity: 0,
    					transform: 'scale(0.95)'
    				},
    				'100%': {
    					opacity: 1,
    					transform: 'scale(1)'
    				}
    			},
    			gradientFlow: {
    				'0%': {
    					backgroundPosition: '0% 50%'
    				},
    				'50%': {
    					backgroundPosition: '100% 50%'
    				},
    				'100%': {
    					backgroundPosition: '0% 50%'
    				}
    			},
    			shimmer: {
    				'0%': {
    					backgroundPosition: '-200% 0'
    				},
    				'100%': {
    					backgroundPosition: '200% 0'
    				}
    			}
    		},
    		animation: {
    			'accordion-down': 'accordion-down 0.2s ease-out',
    			'accordion-up': 'accordion-up 0.2s ease-out',
    			fadeSlideIn: 'fadeSlideIn 0.3s ease-out forwards',
    			slideFromLeft: 'slideFromLeft 0.3s ease-out forwards',
    			floatIn: 'floatIn 0.8s ease-out forwards',
    			slideInRight: 'slideInRight 0.6s ease-out forwards',
    			fadeScale: 'fadeScale 0.6s ease-out forwards',
    			gradientFlow: 'gradientFlow 15s ease infinite',
    			shimmer: 'shimmer 3s linear infinite'
    		}
    	}
    },
    plugins: [
        animatePlugin,
        function ({ addUtilities }) {
            const newUtilities = {
                '.animation-delay-100': {
                    'animation-delay': '0.1s',
                },
                '.animation-delay-200': {
                    'animation-delay': '0.2s',
                },
                '.animation-delay-300': {
                    'animation-delay': '0.3s',
                },
                '.animation-delay-400': {
                    'animation-delay': '0.4s',
                },
                '.animation-delay-500': {
                    'animation-delay': '0.5s',
                },
            }
            addUtilities(newUtilities)
        }
    ]
}
