import fs from 'fs';
import path from 'path';

const CACHE_DIR = path.join(import.meta.dirname, 'cache');

export async function fetchDirectoryContents (owner, repo, directoryPath, accessToken = null) {
  const url = `https://api.github.com/repos/${owner}/${repo}/contents/${directoryPath}`;
  
  const headers = {
    Accept: "application/vnd.github.v3+json",
  };

  // Include authorization header if an access token is provided
  if (accessToken) {
    headers.Authorization = `Bearer ${accessToken}`;
  }

  try {
    const slug = `${directoryPath.replace(/\//g, '-')}.json`;
    const cachePath = path.join(CACHE_DIR, slug);
    let data;
    if (fs.existsSync(cachePath)) {
      console.info(`Using cached response from '${slug}'`);
      data = JSON.parse(fs.readFileSync(cachePath, 'utf8'));
    } else {
      console.info(`Fetching ${url}`);
      const response = await fetch(url, { headers });

      if (!response.ok) {
        throw new Error(`Error: ${response.status} - ${response.statusText}`);
      }

      data = await response.json();

      fs.writeFileSync(cachePath, JSON.stringify(data));
    }

    if (Array.isArray(data)) {
      return data;
    } else {
      console.log("The specified path is not a directory.");
    }
  } catch (error) {
    console.error(`Failed to fetch directory contents: ${error.message}`);
  }
};

