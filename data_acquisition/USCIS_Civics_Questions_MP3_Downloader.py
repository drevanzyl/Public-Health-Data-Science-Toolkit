import requests
from bs4 import BeautifulSoup
import os
import re

def download_mp3(url, filename):
    response = requests.get(url)
    if response.status_code == 200:
        with open(filename, 'wb') as f:
            f.write(response.content)
        print(f"Downloaded: {filename}")
    else:
        print(f"Failed to download: {filename}")

def main():
    url = "https://www.uscis.gov/citizenship/find-study-materials-and-resources/study-for-the-test/100-civics-questions-and-answers-with-mp3-audio-english-version"
    
    # Create a directory to store the MP3 files
    if not os.path.exists("civics_mp3"):
        os.makedirs("civics_mp3")

    # Fetch the webpage content
    response = requests.get(url)
    soup = BeautifulSoup(response.text, 'html.parser')

    # Find all links that end with .mp3
    mp3_links = soup.find_all('a', href=re.compile(r'\.mp3$'))

    for link in mp3_links:
        mp3_url = link['href']
        if not mp3_url.startswith('http'):
            mp3_url = f"https://www.uscis.gov{mp3_url}"
        
        # Extract filename from URL
        filename = os.path.join("civics_mp3", mp3_url.split('/')[-1])
        
        download_mp3(mp3_url, filename)

    print("Download complete!")

if __name__ == "__main__":
    main()